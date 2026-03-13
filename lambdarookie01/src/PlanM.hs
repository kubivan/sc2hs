module PlanM where

import Actions
import AgentBulidUtils (agentUnitCost, canAfford, findBuilder, findFreeGeyser, findPlacementPos, pylonRadius)
import BotDynamicState (HasBuildIntents, HasReservedCost, agentGetBuildIntents, agentModifyBuildIntents, agentModifyReservedCost)
import Conduit (filterC, (.|))
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.HashMap.Strict qualified as HashMap
import Debug.Trace (traceM)
import Footprint (getFootprint)
import Intent
import Lens.Micro ((&), (.~), (^.))
import Observation
import SC2.Geometry (Pointable, distSquared, fromTuple)
import SC2.Grid
import SC2.Ids.AbilityId (AbilityId, isBuildAbility)
import SC2.Ids.Ids
import SC2.TechTree (unitToAbility)
import Squad (Target (..))
import StepMonad
import Units (Unit, mapTilePosC, runC, unitTypeC)
import Utils

type BuildOrder = [UnitTypeId]

reserveLand :: (HasGrid d) => UnitTypeId -> TilePos -> StepMonad d ()
reserveLand uid pos = agentModifyGrid (\g -> addMark g (getFootprint uid) pos)

issueBuildIntent :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => UnitTypeId -> AbilityId -> Unit -> Maybe TilePos -> Action -> StepMonad d ()
issueBuildIntent uid ability builder maybePos action = do
    cost <- agentUnitCost uid
    obs <- agentObs
    let frameIssued = obs ^. #gameLoop
        intentId = (builder ^. #tag, ability)
        buildCmd =
            IntentBuildCommand
                { ibcExecutor = builder ^. #tag
                , ibcAbility = ability
                , ibcUnitType = uid
                , ibcTarget = fmap TargetPos maybePos
                }
        rollbackStack = [IntentBuildAction buildCmd, IntentReserveAction cost]
        intent =
            BuildIntent
                { biActions = [IntentReserveAction cost, IntentBuildAction buildCmd]
                , biRollbackStack = rollbackStack
                , biReservedCost = cost
                , biIssuedAtFrame = frameIssued
                , biState = IntentIssued
                , biRollbackReason = Nothing
                }

    mapM_ (reserveLand uid) maybePos
    agentModifyReservedCost (+ cost)
    agentModifyBuildIntents (HashMap.insert intentId intent)
    command [action]

rollbackIntentAction :: (HasGrid d, HasReservedCost d) => IntentAction -> StepMonad d ()
rollbackIntentAction (IntentReserveAction cost) = agentModifyReservedCost (\c -> c - cost)
rollbackIntentAction (IntentBuildAction cmd) =
    case ibcTarget cmd of
        Just (TargetPos pos) -> agentModifyGrid (\g -> removeMark g (getFootprint (ibcUnitType cmd)) pos)
        _ -> pure ()

rollbackBuildIntent :: (HasGrid d, HasBuildIntents d, HasReservedCost d) => BuildRollbackReason -> BuildIntentId -> StepMonad d ()
rollbackBuildIntent reason intentId = do
    store <- agentGetBuildIntents
    case HashMap.lookup intentId store of
        Nothing -> pure ()
        Just intent -> do
            mapM_ rollbackIntentAction (biRollbackStack intent)
            agentModifyBuildIntents $ HashMap.adjust (\bi -> bi{biState = IntentRolledBack, biRollbackReason = Just reason}) intentId

confirmBuildIntent :: (HasBuildIntents d, HasReservedCost d) => BuildIntentId -> StepMonad d ()
confirmBuildIntent intentId = do
    store <- agentGetBuildIntents
    case HashMap.lookup intentId store of
        Nothing -> pure ()
        Just bi -> do
            when (biState bi /= IntentRolledBack) $ agentModifyReservedCost (\c -> c - biReservedCost bi)
            agentModifyBuildIntents
                ( HashMap.adjust
                    (\x -> if biState x == IntentRolledBack then x else x{biState = IntentConfirmed})
                    intentId
                )

splitAffordable :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => BuildOrder -> StepMonad d BuildOrder
splitAffordable bo = runBO bo

runBO :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => BuildOrder -> StepMonad d BuildOrder
runBO [] = pure []
runBO (u : us) = do
    ok <- tryCreate u
    case ok of
        Nothing -> pure (u : us)
        Just _ -> pure us
tryCreate uid = runMaybeT $ createAction uid

createAction :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => UnitTypeId -> MaybeStepMonad d ()
createAction order = buildAction order -- <|> pylonBuildAction

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
