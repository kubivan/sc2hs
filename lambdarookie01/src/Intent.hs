module Intent where

import Actions (Action, UnitTag, getCmd, getExecutors)
import Observation (Cost)
import SC2.Grid (TilePos)
import SC2.Ids.AbilityId (AbilityId)
import SC2.Ids.UnitTypeId (UnitTypeId)
import Squad(Target(..))

import Data.HashMap.Strict (HashMap)
import Data.Word (Word32)
import Lens.Micro ((^.), Lens')
import Data.Text (Text)
import StepMonad
import SC2.Geometry (Pointable)
import Data.Map
import StepMonad (HasReservedCost)

import Control.Monad.Free

newtype IntentId = IntentId Text
  deriving (Eq, Ord, Show)

class HasBuildIntents d where
  buildIntentsL :: Lens' d Map IntentId (Intent d)

reserveLand :: (HasGrid d) => UnitTypeId -> TilePos -> StepMonad d ()
reserveLand uid pos = agentModifyGrid (\g -> addMark g (getFootprint uid) pos)

reserveCost :: (HasReservedCost d) => UnitTypeId -> StepMonad d ()
reserveCost uid = do 
  c <- agentUnitCost uid
  agentModifyReservedCost (\rc -> rc + c)

data IntentStatus 
  = IntentSpawned 
  | IntentRunning
  | IntentCompleted
  | IntentFailed
  deriving (Eq, Show)

type IntentDSL d = Free (IntentF)

data IntentF next 
  = WaitUntil (StepMonad d Bool) next
  | FindBuilder (UnitTag -> next)
  | FindPlacement UnitTypeId (TileTos -> next)
  | IssueBuild UnitTag UnitTypeID TilePos next
  | ReserveCost UnitTypeID next
  | Guard (StepMonad d Bool) next
  | Complete

waitUntil cond = liftF (WaitUntil cond ())

findBuilder = liftF (FindBuilder id)

findPlacement = liftF (FindPlacement b id)

issueBuild b uid pos =
  liftF (IssueBuild b uid pos ())

reserveCost uid =
  liftF (ReserveCost uid ())

guardTech cond =
  liftF (Guard cond ())

data IntentRuntime d =
  IntentRuntime 
    { intentID :: IntentId
    , intentProg :: IntentDSL d () -- intentProg is the remaining program.
    }

runIntentStep
 :: IntentRuntime d
 -> StepMonad d (IntentRuntime d, IntentStatus)
runIntentStep (IntentRuntime iid prog) =
  case prog of
    Pure _ ->
      pure (IntentRuntime iid prog, IntentCompleted)

    Free instr ->
      case instr of
        WaitUntil cond next -> do
          ok <- cond
          if ok
            then pure (IntentRuntime iid next, IntentRunning)
            else pure (IntentRuntime iid prog, IntentRunning)

        ReserveCost uid next -> do
          reserveCost uid
          pure (IntentRuntime iid next, IntentRunning)

        FindBuilder k -> do
          mb <- agentFindBuilder

          case mb of
            Nothing -> 
              pure (IntentRuntime iid prog, IntentRunning) --TOD0: error handling
            Just b ->
              pure (IntentRuntime iid (k b), IntentRunning)

        IssueBuild b uid pos next -> do
          issueBuild b uid pos
          pure (IntentRuntime iid next, IntentRunning)

        Guard cond next -> do
          ok <- cond

          if ok
            then pure (IntentRuntime iid next, IntentRunning)
            else pure (IntentRuntime iid prog, IntentFailed)

        Complete ->
          pure (IntentRuntime iid prog, IntentCompleted)

data Intent d = Intent
  { intentId :: IntentId
  , intentProgram :: IntentDSL d
  , intentStartedFrame :: Word32
  }

spawnIntent
  :: IntentId 
  -> IntentDSL d
  -> StepMonad d ()
spawnIntent iid prog = 
  modify $ \ds ->
    ds
      { dsIntents = Map.insert iid 
          Map.insert iid 
            (Intent iid prog 0) #TODO: corrent frame
            (dsIntents ds)
      }

intentProcess :: StepMonad d ()
intentProcess = do
  intents <- gets dsIntents
  forM_ (Map.toList intents) $ \(iid, intent) -> do
    (intent', status) <- runIntentStep intent
    updateIntent intent'

    case status of
      IntentRunning -> pure ()
      IntentCompleted -> removeIntent iid
      IntentFailed -> removeIntent iid #TODO: rollbackIntent

removeIntent :: IntentId -> StepMonad d ()
removeIntent iid =
  modify $ \ds ->
    ds { dsIntents = Map.delete iid (dsIntents ds) }


inBuildThechTree :: UnitTypeId -> StepMonad d Bool
inBuildThechTree uid = do
    abilities <- agentAbilities
    si <- agentStatic
    let ability = unitToAbility (unitTraits si) uid
    return $ ability `elem` (abilities HashMap.! ProtossProbe)

distantEnough :: (Foldable t, Pointable p1, Pointable p2) => t p2 -> Float -> p1 -> Bool
distantEnough units radius pos = all (\p -> distSquared pos p >= radius * radius) units

guardStepM :: StepMonad d Bool -> MaybeStepMonad d ()
guardStepM action = lift action >>= guard

pylonBuildAction :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => MaybeStepMonad d ()
pylonBuildAction = do
    guardStepM (canAfford ProtossPylon)
    si <- lift agentStatic
    obs <- lift agentObs
    grid <- lift agentGrid
    let hasPylonsInProgress =
            not
                $ Prelude.null
                $ runC
                $ unitsSelf obs
                    .| unitTypeC ProtossPylon
                    .| filterC (\u -> u ^. #buildProgress < 1)
    guard (not hasPylonsInProgress)
    builder <- MaybeT . return $ findBuilder obs
    let findPylonPlacement = findPlacementPoint grid (heightMap si) (getFootprint ProtossPylon) (tilePos (builder ^. #pos))
        pylonsPos = runC $ unitsSelf obs .| unitTypeC ProtossPylon .| mapTilePosC
        pylonCriteria = distantEnough pylonsPos
    pylonPos <- MaybeT . return $ findPylonPlacement (pylonCriteria pylonRadius)
    command [PointCommand PROTOSSBUILDPYLON [builder] (fromTuple pylonPos)]

            b uid pos
issueBuild :: Unit -> UnitTypeId -> TilePos -> StepMonad d ()
issueBuild ProtossAssimilator = do
    geyser <- findFreeGeyser obs
    --TODO: issue build should work with Target
    command [UnitCommand PROTOSSBUILDASSIMILATOR [builder] (fromJust geyser)]

issueBuild builder order pos = do
    si <- agentStatic
    -- obs <- agentObs
    -- let ability = unitToAbility (unitTraits si) order
    -- --guard (isBuildAbility ability)
    -- --pos <- MaybeT . return $ findPlacementPos obs (expandsPos si) grid (heightMap si) order
    traceM $ show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!"
    command [PointCommand ability [builder] (fromTuple pos)]
