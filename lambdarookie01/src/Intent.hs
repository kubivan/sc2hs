module Intent where

import Actions (Action (PointCommand, SelfCommand, UnitCommand), UnitTag)
import Conduit (filterC, (.|))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Function (on)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Word (Word32)
import Footprint (getFootprint)
import Lens.Micro (Lens', (%~), (^.))
import Observation (Cost (..), Observation, findNexus, obsResources, obsUnitsC, unitsSelf)
import SC2.Geometry (distSquared, fromTuple)
import SC2.Grid (Grid, TilePos, addMark, canPlaceBuilding, findPlacementPoint, findPlacementPointInRadius, tilePos)
import SC2.Ids.AbilityId (AbilityId (HARVESTGATHERPROBE))
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossAssimilator, ProtossNexus, ProtossProbe, ProtossPylon))
import SC2.TechTree (UnitTraits, abilityExecutor, unitToAbility)
import StepMonad
  ( HasGrid
  , HasObs
  , MaybeStepMonad
  , StepMonad
  , UnitTraits
  , agentGet
  , agentAbilities
  , agentGrid
  , agentModify
  , agentModifyGrid
  , agentObs
  , agentStatic
  , command
  , expandsPos
  , heightMap
  , unitTraits
  )
import Units (Unit, isGeyser, mapTilePosC, runC, toEnum', unitTypeC)

import Data.Set qualified as Set

newtype IntentId = IntentId Text
  deriving (Eq, Ord, Show)

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free next) = Free (fmap (fmap f) next)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> mx = fmap f mx
  Free next <*> mx = Free (fmap (<*> mx) next)

instance Functor f => Monad (Free f) where
  Pure a >>= k = k a
  Free next >>= k = Free (fmap (>>= k) next)

liftF :: Functor f => f a -> Free f a
liftF fa = Free (Pure <$> fa)

data IntentStatus
  = IntentRunning
  | IntentCompleted
  | IntentFailed
  deriving (Eq, Show)

data IntentF d next
  = WaitUntil (StepMonad d Bool) next
  | ReserveCost UnitTypeId next
  | ReleaseCost UnitTypeId next
  | FindBuilder (UnitTag -> next)
  | FindPlacement UnitTypeId (TilePos -> next)
  | FindGeyser (UnitTag -> next)
  | FindProducerForUnit UnitTypeId (UnitTag -> next)
  | IssuePointBuild UnitTag UnitTypeId TilePos next
  | IssueUnitBuild UnitTag UnitTypeId UnitTag next
  | IssueSelfCommand UnitTag UnitTypeId next

instance Functor (IntentF d) where
  fmap f (WaitUntil cond next) = WaitUntil cond (f next)
  fmap f (ReserveCost uid next) = ReserveCost uid (f next)
  fmap f (ReleaseCost uid next) = ReleaseCost uid (f next)
  fmap f (FindBuilder k) = FindBuilder (f . k)
  fmap f (FindPlacement uid k) = FindPlacement uid (f . k)
  fmap f (FindGeyser k) = FindGeyser (f . k)
  fmap f (FindProducerForUnit uid k) = FindProducerForUnit uid (f . k)
  fmap f (IssuePointBuild builder uid pos next) = IssuePointBuild builder uid pos (f next)
  fmap f (IssueUnitBuild builder uid target next) = IssueUnitBuild builder uid target (f next)
  fmap f (IssueSelfCommand producer uid next) = IssueSelfCommand producer uid (f next)

type IntentDSL d a = Free (IntentF d) a

data IntentRuntime d = IntentRuntime
  { intentId :: IntentId
  , intentProgram :: IntentDSL d ()
  , intentStartedFrame :: Word32
  }

type IntentStore d = Map IntentId (IntentRuntime d)

class HasBuildIntents d where
  buildIntentsL :: Lens' d (IntentStore d)

waitUntil :: StepMonad d Bool -> IntentDSL d ()
waitUntil cond = liftF (WaitUntil cond ())

reserveCostI :: UnitTypeId -> IntentDSL d ()
reserveCostI uid = liftF (ReserveCost uid ())

releaseCostI :: UnitTypeId -> IntentDSL d ()
releaseCostI uid = liftF (ReleaseCost uid ())

findBuilderI :: IntentDSL d UnitTag
findBuilderI = liftF (FindBuilder id)

findPlacementI :: UnitTypeId -> IntentDSL d TilePos
findPlacementI uid = liftF (FindPlacement uid id)

findGeyserI :: IntentDSL d UnitTag
findGeyserI = liftF (FindGeyser id)

findProducerForI :: UnitTypeId -> IntentDSL d UnitTag
findProducerForI uid = liftF (FindProducerForUnit uid id)

issuePointBuildI :: UnitTag -> UnitTypeId -> TilePos -> IntentDSL d ()
issuePointBuildI builder uid pos = liftF (IssuePointBuild builder uid pos ())

issueUnitBuildI :: UnitTag -> UnitTypeId -> UnitTag -> IntentDSL d ()
issueUnitBuildI builder uid target = liftF (IssueUnitBuild builder uid target ())

issueSelfCommandI :: UnitTag -> UnitTypeId -> IntentDSL d ()
issueSelfCommandI producer uid = liftF (IssueSelfCommand producer uid ())

reserveCostStep :: UnitTypeId -> StepMonad d ()
reserveCostStep _ = pure ()

releaseCostStep :: UnitTypeId -> StepMonad d ()
releaseCostStep _ = pure ()

abilityAvailable :: UnitTypeId -> StepMonad d Bool
abilityAvailable uid = do
  si <- agentStatic
  abilities <- agentAbilities
  let ability = unitToAbility (unitTraits si) uid
      executor = abilityExecutor HashMap.! ability
  pure $ ability `elem` HashMap.lookupDefault [] executor abilities

findProducerTag :: HasObs d => UnitTypeId -> StepMonad d (Maybe UnitTag)
findProducerTag producerType = do
  obs <- agentObs
  pure $ (^. #tag) <$> find ((== 1) . (^. #buildProgress)) (runC $ unitsSelf obs .| unitTypeC producerType)

agentUnitCost :: UnitTypeId -> StepMonad d Cost
agentUnitCost uid = do
  si <- agentStatic
  pure $ unitCost (unitTraits si) uid

unitCost :: UnitTraits -> UnitTypeId -> Cost
unitCost traits uid =
  case traits HashMap.!? uid of
    Just trait -> Cost (fromIntegral $ trait ^. #mineralCost) (fromIntegral $ trait ^. #vespeneCost)
    Nothing -> Cost 0 0

canAffordNow :: HasObs d => UnitTypeId -> StepMonad d Bool
canAffordNow uid = do
  si <- agentStatic
  obs <- agentObs
  pure $ obsResources obs >= unitCost (unitTraits si) uid

agentFindBuilder :: HasObs d => StepMonad d (Maybe Unit)
agentFindBuilder = findBuilder <$> agentObs

findBuilder :: Observation -> Maybe Unit
findBuilder obs =
  find availableProbe (runC $ unitsSelf obs .| unitTypeC ProtossProbe)
  where
    availableProbe :: Unit -> Bool
    availableProbe unit =
      Prelude.null (unit ^. #orders)
        || (length (unit ^. #orders) == 1 && HARVESTGATHERPROBE `elem` map (toEnum' . (^. #abilityId)) (unit ^. #orders))

findPlacementPos :: Observation -> [TilePos] -> Grid -> Grid -> UnitTypeId -> Maybe TilePos
findPlacementPos _ expands grid heightMap ProtossNexus = find (\pos -> canPlaceBuilding grid heightMap pos (getFootprint ProtossNexus)) expands
findPlacementPos obs _ grid heightMap ProtossPylon =
  findPlacementPoint grid heightMap (getFootprint ProtossPylon) nexusPos (const True)
  where
    nexusPos = tilePos $ findNexus obs ^. #pos
findPlacementPos obs _ grid heightMap uid = go pylons
  where
    go (p : ps) =
      case findPlacementPointInRadius grid heightMap (getFootprint uid) p 6.5 of
        Just res -> Just res
        Nothing -> go ps
    go [] = Nothing
    pylons =
      runC $
        unitsSelf obs
          .| unitTypeC ProtossPylon
          .| mapTilePosC

findFreeGeyser :: Observation -> Maybe Unit
findFreeGeyser obs = find (\u -> not (tilePos (u ^. #pos) `Set.member` assimilatorPositions)) geysersSorted
  where
    assimilatorPositions = Set.fromList $ runC $ unitsSelf obs .| unitTypeC ProtossAssimilator .| mapTilePosC
    nexusPos = tilePos $ findNexus obs ^. #pos
    geysersSorted =
      sortBy (compare `on` (\u -> (u ^. #pos) `distSquared` nexusPos))
        (runC $ obsUnitsC obs .| filterC isGeyser)

runIntentStep
  :: (HasObs d, HasGrid d)
  => IntentRuntime d
  -> StepMonad d (IntentRuntime d, IntentStatus)
runIntentStep runtime@(IntentRuntime iid program startedFrame) =
  case program of
    Pure _ -> pure (runtime, IntentCompleted)
    Free instruction ->
      case instruction of
        WaitUntil cond next -> do
          ready <- cond
          if ready
            then pure (IntentRuntime iid next startedFrame, IntentRunning)
            else pure (runtime, IntentRunning)

        ReserveCost uid next -> do
          reserveCostStep uid
          pure (IntentRuntime iid next startedFrame, IntentRunning)

        ReleaseCost uid next -> do
          releaseCostStep uid
          pure (IntentRuntime iid next startedFrame, IntentRunning)

        FindBuilder k -> do
          mbBuilder <- agentFindBuilder
          case mbBuilder of
            Nothing -> pure (runtime, IntentRunning)
            Just builder -> pure (IntentRuntime iid (k (builder ^. #tag)) startedFrame, IntentRunning)

        FindPlacement uid k -> do
          obs <- agentObs
          grid <- agentGrid
          si <- agentStatic
          case findPlacementPos obs (expandsPos si) grid (heightMap si) uid of
            Nothing -> pure (runtime, IntentRunning)
            Just pos -> pure (IntentRuntime iid (k pos) startedFrame, IntentRunning)

        FindGeyser k -> do
          obs <- agentObs
          case findFreeGeyser obs of
            Nothing -> pure (runtime, IntentRunning)
            Just geyser -> pure (IntentRuntime iid (k (geyser ^. #tag)) startedFrame, IntentRunning)

        FindProducerForUnit uid k -> do
          si <- agentStatic
          let ability = unitToAbility (unitTraits si) uid
              producerType = abilityExecutor HashMap.! ability
          producer <- findProducerTag producerType
          case producer of
            Nothing -> pure (runtime, IntentRunning)
            Just tag -> pure (IntentRuntime iid (k tag) startedFrame, IntentRunning)

        IssuePointBuild builder uid pos next -> do
          si <- agentStatic
          obs <- agentObs
          let ability = unitToAbility (unitTraits si) uid
          case find ((== builder) . (^. #tag)) (obs ^. #rawData . #units) of
            Nothing -> pure (runtime, IntentRunning)
            Just executor -> do
              agentModifyGrid (\grid -> addMark grid (getFootprint uid) pos)
              command [PointCommand ability [executor] (fromTuple pos)]
              pure (IntentRuntime iid next startedFrame, IntentRunning)

        IssueUnitBuild builder uid target next -> do
          si <- agentStatic
          obs <- agentObs
          let ability = unitToAbility (unitTraits si) uid
          case ( find ((== builder) . (^. #tag)) (obs ^. #rawData . #units)
               , find ((== target) . (^. #tag)) (obs ^. #rawData . #units)
               ) of
            (Just executor, Just targetUnit) -> do
              command [UnitCommand ability [executor] targetUnit]
              pure (IntentRuntime iid next startedFrame, IntentRunning)
            _ -> pure (runtime, IntentRunning)

        IssueSelfCommand producer uid next -> do
          si <- agentStatic
          obs <- agentObs
          let ability = unitToAbility (unitTraits si) uid
          case find ((== producer) . (^. #tag)) (obs ^. #rawData . #units) of
            Nothing -> pure (runtime, IntentRunning)
            Just executor -> do
              command [SelfCommand ability [executor]]
              pure (IntentRuntime iid next startedFrame, IntentRunning)

spawnIntent :: (HasObs d, HasBuildIntents d) => IntentId -> IntentDSL d () -> StepMonad d ()
spawnIntent iid program = do
  frame <- (^. #gameLoop) <$> agentObs
  agentModify (buildIntentsL %~ Map.insert iid (IntentRuntime iid program frame))

intentExists :: HasBuildIntents d => IntentId -> StepMonad d Bool
intentExists iid = Map.member iid . (^. buildIntentsL) <$> agentGet

lookupIntent :: HasBuildIntents d => IntentId -> StepMonad d (Maybe (IntentRuntime d))
lookupIntent iid = Map.lookup iid . (^. buildIntentsL) <$> agentGet

updateIntent :: HasBuildIntents d => IntentRuntime d -> StepMonad d ()
updateIntent runtime =
  agentModify (buildIntentsL %~ Map.insert (intentId runtime) runtime)

removeIntent :: HasBuildIntents d => IntentId -> StepMonad d ()
removeIntent iid =
  agentModify (buildIntentsL %~ Map.delete iid)

stepIntent
  :: (HasObs d, HasGrid d, HasBuildIntents d)
  => IntentId
  -> StepMonad d IntentStatus
stepIntent iid = do
  current <- lookupIntent iid
  case current of
    Nothing -> pure IntentFailed
    Just runtime -> do
      (runtime', status) <- runIntentStep runtime
      case status of
        IntentRunning -> updateIntent runtime' >> pure IntentRunning
        IntentCompleted -> removeIntent iid >> pure IntentCompleted
        IntentFailed -> removeIntent iid >> pure IntentFailed

ensureStructure :: HasObs d => UnitTypeId -> IntentDSL d ()
ensureStructure uid = do
  waitUntil (canAffordNow uid)
  waitUntil (abilityAvailable uid)
  reserveCostI uid
  builder <- findBuilderI
  if uid == ProtossAssimilator
    then do
      geyser <- findGeyserI
      issueUnitBuildI builder uid geyser
    else do
      pos <- findPlacementI uid
      issuePointBuildI builder uid pos
  releaseCostI uid

ensureUnit :: HasObs d => UnitTypeId -> IntentDSL d ()
ensureUnit uid = do
  waitUntil (canAffordNow uid)
  waitUntil (abilityAvailable uid)
  reserveCostI uid
  producer <- findProducerForI uid
  issueSelfCommandI producer uid
  releaseCostI uid

transientStep
  :: (HasObs d, HasGrid d)
  => IntentDSL d ()
  -> StepMonad d IntentStatus
transientStep program = snd <$> runIntentStep (IntentRuntime (IntentId (pack "transient")) program 0)

pylonBuildAction :: (HasObs d, HasGrid d) => MaybeStepMonad d ()
pylonBuildAction = do
  affordable <- lift $ canAffordNow ProtossPylon
  MaybeT $ pure (if affordable then Just () else Nothing)
  lift $ do
    _ <- transientStep (ensureStructure ProtossPylon)
    pure ()
