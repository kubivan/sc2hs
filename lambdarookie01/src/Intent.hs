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

newtype IntentId = IntentId Text
  deriving (Eq, Ord, Show)

class HasBuildIntents d where
  buildIntentsL :: Lens' d Map IntentId (Intent d)

data IntentStatus 
  = IntentSpawned 
  | IntentRunning
  | IntentCompleted
  | IntentFailed
  deriving (Eq, Show)

newtype IntentDSL d =
  IntentDSL { runIntentDSL :: StepMonad d IntentStatus }

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
    status <- runIntentDSL (intentProgram intent)

    case status of
      IntentRunning -> pure ()
      IntentCompleted -> removeIntent iid
      IntentFailed -> removeIntent iid #TODO: rollbackIntent

removeIntent :: IntentId -> StepMonad d ()
removeIntent iid =
  modify $ \ds ->
    ds { dsIntents = Map.delete iid (dsIntents ds) }

data IntentEnv = IntentEnvBuild UnitTypeId AbilityId UnitTag (Maybe Target)

reserveLand :: (HasGrid d) => UnitTypeId -> TilePos -> StepMonad d ()
reserveLand uid pos = agentModifyGrid (\g -> addMark g (getFootprint uid) pos)

reserveCost :: (HasReservedCost d) => UnitTypeId -> StepMonad d ()
reserveCost uid = do 
  c <- agentUnitCost uid
  agentModifyReservedCost (\rc -> rc + c)

intentToBuild
  :: (HasObs d, HasGrid d)
  => UnitTypeId
  -> MaybeStepMonad IntentEnv -- correct ret value should be determined
intentToBuild uid = do
  checkTimeout -- return if stepcount limit is over
  reserveCost -- should also modify intentenv - we reserved the cost for it
  waitUntil(canAfford uid) -- we should return Pending until res avail

  builder <- MaybeT $ agentFindBuilder -- return pending if builder not avail
  --here we need to update the intent: add builder tag to the intentCtx
  ability <- abilityForUnit ut
  let env = IntentEnv (builder ^. #unitTag) uid ability -- we have all 
  --issueBuild worker ability pos
  command [..]

      pure IntentRunning


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
    let action = PointCommand PROTOSSBUILDPYLON [builder] (fromTuple pylonPos)
    lift $ issueBuildIntent ProtossPylon PROTOSSBUILDPYLON builder (Just pylonPos) action

buildAction :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => UnitTypeId -> MaybeStepMonad d ()
buildAction ProtossAssimilator = do
    guardStepM (canAfford ProtossAssimilator)
    obs <- lift agentObs
    builder <- MaybeT . return $ findBuilder obs
    geyser <- MaybeT . return $ findFreeGeyser obs
    let action = UnitCommand PROTOSSBUILDASSIMILATOR [builder] geyser
    lift $ issueBuildIntent ProtossAssimilator PROTOSSBUILDASSIMILATOR builder Nothing action

buildAction order = do
    guardStepM (inBuildThechTree order)
    guardStepM (canAfford order)
    si <- lift agentStatic
    obs <- lift agentObs
    grid <- lift agentGrid
    let ability = unitToAbility (unitTraits si) order
    guard (isBuildAbility ability)
    builder <- MaybeT . return $ findBuilder obs
    pos <- MaybeT . return $ findPlacementPos obs (expandsPos si) grid (heightMap si) order
    traceM $ show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!"
    let action = PointCommand ability [builder] (fromTuple pos)
    lift $ issueBuildIntent order ability builder (Just pos) action
