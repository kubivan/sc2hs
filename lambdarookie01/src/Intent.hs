module Intent where

import Actions (Action (PointCommand, SelfCommand, UnitCommand), UnitTag)
import Conduit (filterC, (.|))
import Control.Monad.Free (Free (..), liftF)
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
import Observation (Cost (..), Observation, findNexus, obsUnitsC, unitsSelf, getUnit)
import SC2.Geometry (fromTuple)
import SC2.Grid (Grid, TilePos, addMark, canPlaceBuilding, findPlacementPoint, findPlacementPointInRadius, tilePos)
import SC2.Spatial qualified as Spatial
import SC2.Ids.AbilityId (AbilityId (HARVESTGATHERPROBE))
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossAssimilator, ProtossNexus, ProtossProbe, ProtossPylon))
import SC2.TechTree (abilityExecutor, unitToAbility)
import Target (Target (..))
import StepMonad
  ( HasGrid
  , HasObs
  , HasReservedCost
  , MaybeStepMonad
  , StepMonad
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
import StepMonad
import Units (Unit, isGeyser, mapTilePosC, runC, toEnum', unitTypeC)
import StepMonadUtils (abilityAvailableForUnit, agentCanAfford, agentCanAffordWith, agentUnitCost)

import Data.Set qualified as Set
import Control.Monad (guard)
import Lens.Micro.Extras (view)

newtype IntentId = IntentId Text
  deriving (Eq, Ord, Show)

data IntentStatus
  = IntentRunning
  | IntentCompleted
  | IntentFailed
  deriving (Eq, Show)

data IntentF d next
  = WaitUntil (IntentRuntime d -> StepMonad d Bool) next
  | ReserveCost UnitTypeId next
  | ReleaseCost UnitTypeId next
  | FindBuilder (UnitTag -> next)
  | FindPlacementTarget UnitTypeId (Target -> next)
  | FindProducerForUnit UnitTypeId (UnitTag -> next)
  | IssueBuild UnitTag UnitTypeId Target next
  | IssueSelfCommand UnitTag UnitTypeId next
  -- | WaitForStart UnitTag AbilityId Target next

instance Functor (IntentF d) where
  fmap f (WaitUntil cond next) = WaitUntil cond (f next)
  fmap f (ReserveCost uid next) = ReserveCost uid (f next)
  fmap f (ReleaseCost uid next) = ReleaseCost uid (f next)
  fmap f (FindBuilder k) = FindBuilder (f . k)
  fmap f (FindPlacementTarget uid k) = FindPlacementTarget uid (f . k)
  fmap f (FindProducerForUnit uid k) = FindProducerForUnit uid (f . k)
  fmap f (IssueBuild builder uid target next) = IssueBuild builder uid target (f next) 
  fmap f (IssueSelfCommand producer uid next) = IssueSelfCommand producer uid (f next) -- fmap f (WaitForStart producer ability target next) = WaitForStart producer ability target (f next)

type IntentDSL d a = Free (IntentF d) a

data IntentTickResult d
  = Continue (IntentRuntime d)
  | Block (IntentRuntime d)
  | Done (IntentRuntime d) IntentStatus

data IntentRuntime d = IntentRuntime
  { intentId :: IntentId
  , intentProgram :: IntentDSL d ()
  , intentStartedFrame :: Word32
  , intentReserve :: Cost
  }

type IntentStore d = Map IntentId (IntentRuntime d)

class HasBuildIntents d where
  buildIntentsL :: Lens' d (IntentStore d)

waitUntil :: StepMonad d Bool -> IntentDSL d ()
waitUntil cond = waitUntilWith (const cond)

waitUntilWith :: (IntentRuntime d -> StepMonad d Bool) -> IntentDSL d ()
waitUntilWith cond = liftF (WaitUntil cond ())

reserveCostI :: UnitTypeId -> IntentDSL d ()
reserveCostI uid = liftF (ReserveCost uid ())

releaseCostI :: UnitTypeId -> IntentDSL d ()
releaseCostI uid = liftF (ReleaseCost uid ())

findBuilderI :: IntentDSL d UnitTag
findBuilderI = liftF (FindBuilder id)

findPlacementTargetI :: UnitTypeId -> IntentDSL d Target
findPlacementTargetI uid = liftF (FindPlacementTarget uid id)

findProducerForI :: UnitTypeId -> IntentDSL d UnitTag
findProducerForI uid = liftF (FindProducerForUnit uid id)

issueBuildI :: UnitTag -> UnitTypeId -> Target -> IntentDSL d ()
issueBuildI builder uid target = liftF (IssueBuild builder uid target ())

issueSelfCommandI :: UnitTag -> UnitTypeId -> IntentDSL d ()
issueSelfCommandI producer uid = liftF (IssueSelfCommand producer uid ())

releaseCostStep :: UnitTypeId -> StepMonad d ()
releaseCostStep _ = pure ()

findProducerTag :: HasObs d => UnitTypeId -> StepMonad d (Maybe UnitTag)
findProducerTag producerType = do
  obs <- agentObs
  pure $ (^. #tag) <$> find ((== 1) . (^. #buildProgress)) (runC $ unitsSelf obs .| unitTypeC producerType)

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
      sortBy (compare `on` (\u -> Spatial.distSquared u nexusPos))
        (runC $ obsUnitsC obs .| filterC isGeyser)

findPlacementTarget :: UnitTypeId -> Observation -> [TilePos] -> Grid -> Grid -> Maybe Target
findPlacementTarget uid obs expands grid gridHeight
  | uid == ProtossAssimilator = TargetUnit <$> findFreeGeyser obs
  | otherwise = TargetPos <$> findPlacementPos obs expands grid gridHeight uid


runIntent
  :: (HasObs d, HasGrid d, HasReservedCost d)
  => IntentRuntime d
  -> StepMonad d (IntentRuntime d, IntentStatus)
runIntent = stepLoop
  where
    stepLoop rt = do
      result <- intentTick rt
      case result of
        Continue rt' -> stepLoop rt'
        Block rt' -> pure (rt', IntentRunning)
        Done rt' status -> pure (rt', status)

intentTick :: (HasObs d, HasGrid d, HasReservedCost d) => IntentRuntime d -> StepMonad d (IntentTickResult d)
intentTick rt =
  case intentProgram rt of
    Pure _ -> pure $ Done rt IntentCompleted
    Free instruction ->
      case instruction of
        WaitUntil cond next -> do
          ready <- cond rt
          if ready
            then pure $ Continue (rt {intentProgram = next})
            else pure $ Block rt

        ReserveCost uid next -> do
          ucost <- agentUnitCost uid
          -- subtract from the global reserve, add to the local
          _ <- agentModifyReservedCost (\c -> c - ucost)
          pure $ Continue (rt {intentProgram = next, intentReserve = (intentReserve rt) + ucost })

        ReleaseCost uid next -> do
          ucost <- agentUnitCost uid
          agentModifyReservedCost (+ ucost)
          pure $ Continue (rt {intentProgram = next, intentReserve = (intentReserve rt) - ucost })

        FindBuilder k -> do
          mbBuilder <- agentFindBuilder
          case mbBuilder of
            Nothing -> pure $ Block rt
            Just builder -> pure $ Continue (rt {intentProgram = k (builder ^. #tag)}) --(IntentRuntime iid (k (builder ^. #tag)) startedFrame, IntentRunning)

        FindPlacementTarget uid k -> do
          obs <- agentObs
          grid <- agentGrid
          si <- agentStatic
          case findPlacementTarget uid obs (expandsPos si) grid (heightMap si) of
            Nothing -> pure $ Block rt
             --IntentRuntime iid (k target) startedFrame, IntentRunning)
            Just target -> pure $ Continue (rt { intentProgram = k target })

        FindProducerForUnit uid k -> do
          si <- agentStatic
          let ability = unitToAbility (unitTraits si) uid
              producerType = abilityExecutor HashMap.! ability
          producer <- findProducerTag producerType
          case producer of
            Nothing -> pure $ Block rt
            Just tag -> pure $ Continue (rt {intentProgram = k tag})

        IssueBuild builder uid target next -> do
          si <- agentStatic
          obs <- agentObs
          let ability = unitToAbility (unitTraits si) uid
          case find ((== builder) . (^. #tag)) (obs ^. #rawData . #units) of
            Nothing -> pure $ Block rt --TODO: fail
            Just executor -> do
              case target of
                TargetPos pos -> do
                  agentModifyGrid (\grid -> addMark grid (getFootprint uid) pos)
                  command [PointCommand ability [executor] (fromTuple pos)]
                  pure $ Continue (rt {intentProgram = next})
                TargetUnit targetUnit -> do
                  command [UnitCommand ability [executor] targetUnit]
                  pure $ Continue (rt {intentProgram = next})

        IssueSelfCommand producer uid next -> do
          si <- agentStatic
          obs <- agentObs
          let ability = unitToAbility (unitTraits si) uid
          case find ((== producer) . (^. #tag)) (obs ^. #rawData . #units) of
            Nothing -> pure $ Block rt --TODO: fail
            Just executor -> do
              command [SelfCommand ability [executor]]
              pure $ Continue (rt {intentProgram = next})

spawnIntent :: (HasObs d, HasBuildIntents d, HasReservedCost d) => IntentId -> IntentDSL d () -> StepMonad d ()
spawnIntent iid program = do
  frame <- (^. #gameLoop) <$> agentObs
  agentModify (buildIntentsL %~ Map.insert iid (IntentRuntime iid program frame (Cost 0 0)))

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
  :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d)
  => IntentId
  -> StepMonad d IntentStatus
stepIntent iid = do
  current <- lookupIntent iid
  case current of
    Nothing -> pure IntentFailed
    Just runtime -> do
      (runtime', status) <- runIntent runtime
      case status of
        IntentRunning -> updateIntent runtime' >> pure IntentRunning
        IntentCompleted -> removeIntent iid >> pure IntentCompleted
        IntentFailed -> do
          agentModifyReservedCost (+ intentReserve runtime')
          removeIntent iid
          pure IntentFailed

--TODO: duplicate
unitHasOrder :: AbilityId -> Units.Unit -> Bool
unitHasOrder order u = order `elem` orders
  where
    orders = toEnum' . view #abilityId <$> u ^. #orders

ensureStructure :: (HasObs d, HasReservedCost d) => UnitTypeId -> IntentDSL d ()
ensureStructure uid = do
  reserveCostI uid
  waitUntilWith (\rt -> agentCanAffordWith (intentReserve rt) uid)
  waitUntil (abilityAvailableForUnit uid)
  builder <- findBuilderI
  target <- findPlacementTargetI uid
  issueBuildI builder uid target
  waitUntil $ do 
    obs <- agentObs 
    si <- agentStatic
    abilities <- agentAbilities
    let ability = unitToAbility (unitTraits si) uid
        mbuilder' = getUnit obs builder
    case mbuilder' of
      Just b -> return $ unitHasOrder ability b
      _ -> pure False
  -- waitUntil $ do 
  --   obs <- agentObs 
  --   si <- agentStatic
  --   abilities <- agentAbilities
  --   let ability = unitToAbility (unitTraits si) uid
  --   mbuilder' <- getUnit obs (builder ^. #tag)
  --   case mbuilder' of
  --     Just b -> return $ unitHasOrder ability
  --     _ -> pure False

  releaseCostI uid

ensureUnit :: (HasObs d, HasReservedCost d) => UnitTypeId -> IntentDSL d ()
ensureUnit uid = do
  waitUntilWith (\rt -> agentCanAffordWith (intentReserve rt) uid)
  waitUntil (abilityAvailableForUnit uid)
  reserveCostI uid
  producer <- findProducerForI uid
  issueSelfCommandI producer uid
  releaseCostI uid

transientStep
  :: (HasObs d, HasGrid d, HasReservedCost d)
  => IntentDSL d ()
  -> StepMonad d IntentStatus
transientStep program = snd <$> runIntent (IntentRuntime (IntentId (pack "transient")) program 0 (Cost 0 0))

pylonBuildAction :: (HasObs d, HasGrid d, HasReservedCost d) => MaybeStepMonad d ()
pylonBuildAction = do
  affordable <- lift $ agentCanAfford ProtossPylon
  MaybeT $ pure (if affordable then Just () else Nothing)
  lift $ do
    _ <- transientStep (ensureStructure ProtossPylon)
    pure ()
