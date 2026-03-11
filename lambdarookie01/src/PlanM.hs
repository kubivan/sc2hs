
module PlanM where

import Actions
import AgentBulidUtils (canAfford, findBuilder, findFreeGeyser, findPlacementPos, pylonRadius)
import Conduit (filterC, (.|))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Writer.Strict (WriterT, listen, runWriterT, tell)
import Data.HashMap.Strict qualified as HashMap
import Debug.Trace (traceM)
import Footprint (getFootprint)
import Lens.Micro ((.~), (^.))
import Observation
import SC2.Geometry (Pointable, distSquared, fromTuple)
import SC2.Grid
import SC2.Ids.AbilityId (isBuildAbility)
import SC2.Ids.Ids
import SC2.TechTree (unitToAbility)
import StepMonad
import Units (mapTilePosC, runC, unitTypeC)
import Utils


data SimState = SimState
  { simGrid :: Grid
  , simReserved :: Cost
  -- , simSupply :: Int
  -- , simWorkersCount :: Int
  }

type BuildOrder = [UnitTypeId]

-- PlanM stacks WriterT [Action] and StateT SimState on top of StepMonad:
--
--   PlanM d
--    ├─ WriterT [Action]          (accumulated build actions)
--    ├─ StateT SimState           (simulated grid + reserved resources)
--    └─ StepMonad d               (agent environment)
--         ├─ WriterT StepPlan
--         ├─ StateT d
--         └─ Reader (StaticInfo, UnitAbilities)
type PlanM d a =
  WriterT [Action] (StateT SimState (StepMonad d)) a

splitAffordable
  :: (HasObs d, HasGrid d)
  => BuildOrder
  -> Cost
  -> StepMonad d ([Action], BuildOrder)
splitAffordable bo reserved = do
  grid <- agentGrid
  let initState = SimState grid reserved
  ((remainingBO, actions), _) <-
    runStateT
      (runWriterT (runBO bo))
      initState
  return (actions, remainingBO)

planStep :: (HasObs d, HasGrid d) => UnitTypeId -> PlanM d Bool
planStep uid = do
  (si, _) <- lift . lift $ agentAsk
  SimState grid reserved <- lift get
  cres <- lift . lift $ tryCreate grid reserved uid
  case cres of
    Nothing ->
      return False
    Just (action, cost, grid') -> do
      tell [action]
      lift $ put SimState
        { simGrid = grid'
        , simReserved = reserved + cost
        }
      return $
        getCmd action == unitToAbility (unitTraits si) uid

runBO :: (HasObs d, HasGrid d) => BuildOrder -> PlanM d BuildOrder
runBO [] = return []
runBO bo@(uid:rest) = do
  consumed <- planStep uid
  if consumed
    then runBO rest
    else do
      actions <- listen (return ()) --TODO: pattern match(r,R)
      case snd actions of
        [] -> return bo
        _ -> runBO bo

tryCreate :: (HasObs d, HasGrid d) => Grid -> Cost -> UnitTypeId -> StepMonad d (Maybe (Action, Cost, Grid))
tryCreate grid reserved uid = runMaybeT $ createAction grid reserved uid

createAction :: (HasObs d, HasGrid d) => Grid -> Cost -> UnitTypeId -> MaybeStepMonad d (Action, Cost, Grid)
createAction grid reserved order = do
  (isAffordable, cost) <- lift $ canAfford order reserved
  guard isAffordable
  buildAction order grid reserved <|> pylonBuildAction grid reserved

inBuildThechTree :: UnitTypeId -> StepMonad d Bool
inBuildThechTree uid = do
  abilities <- agentAbilities
  si        <- agentStatic
  let ability = unitToAbility (unitTraits si) uid
  return $ ability `elem` (abilities HashMap.! ProtossProbe)

distantEnough :: (Foldable t, Pointable p1, Pointable p2) => t p2 -> Float -> p1 -> Bool
distantEnough units radius pos = all (\p -> distSquared pos p >= radius * radius) units

pylonBuildAction :: (HasObs d, HasGrid d) => Grid -> Cost -> MaybeStepMonad d (Action, Cost, Grid)
pylonBuildAction grid reservedRes = do
  (isAffordable, cost) <- lift $ canAfford ProtossPylon reservedRes
  guard isAffordable
  si      <- lift agentStatic
  obs     <- lift agentObs
  let hasPylonsInProgress = not $ Prelude.null $ runC $
        unitsSelf obs .| unitTypeC ProtossPylon .| filterC (\u -> u ^. #buildProgress < 1)
  guard (not hasPylonsInProgress)
  builder <- MaybeT . return $ findBuilder obs
  let footprint          = getFootprint ProtossPylon
      findPylonPlacement = findPlacementPoint grid (heightMap si) footprint (tilePos (builder ^. #pos))
      pylonsPos          = runC $ unitsSelf obs .| unitTypeC ProtossPylon .| mapTilePosC
      pylonCriteria      = distantEnough pylonsPos
  pylonPos <- MaybeT . return $ findPylonPlacement (pylonCriteria pylonRadius)
  let grid' = addMark grid footprint pylonPos
      obs'  = addOrder (builder ^. #tag) PROTOSSBUILDPYLON obs
  lift $ agentModify ((obsL .~ obs') . (gridL .~ grid'))
  return (PointCommand PROTOSSBUILDPYLON [builder] (fromTuple pylonPos), cost, grid')
    `Utils.dbg` ("pylonPos: " ++ show pylonPos)

buildAction :: (HasObs d, HasGrid d) => UnitTypeId -> Grid -> Cost -> MaybeStepMonad d (Action, Cost, Grid)
buildAction ProtossAssimilator grid reservedRes = do
  (isAffordable, cost) <- lift $ canAfford ProtossAssimilator reservedRes
  guard isAffordable
  obs     <- lift agentObs
  builder <- MaybeT . return $ findBuilder obs
  geyser  <- MaybeT . return $ findFreeGeyser obs
  let res = UnitCommand PROTOSSBUILDASSIMILATOR [builder] geyser
  return (res, cost, grid)
buildAction order _ reservedRes = do
  enabled <- lift $ inBuildThechTree order
  guard enabled
  (isAffordable, cost) <- lift $ canAfford order reservedRes
  guard isAffordable
  si      <- lift agentStatic
  obs     <- lift agentObs
  grid    <- lift agentGrid
  let ability   = unitToAbility (unitTraits si) order
      footprint = getFootprint order
  guard (isBuildAbility ability)
  builder <- MaybeT . return $ findBuilder obs
  pos     <- MaybeT . return $ findPlacementPos obs (expandsPos si) grid (heightMap si) order
  let grid' = addMark grid footprint pos
      obs'  = addOrder (builder ^. #tag) ability . addUnit order $ obs
  traceM $ show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!"
  lift $ agentModifyObs (const obs')
  lift $ agentModifyGrid (const grid')
  let res = PointCommand ability [builder] (fromTuple pos)
  return (res, cost, grid') `Utils.dbg` ("builder orders " ++ show (builder ^. #orders))
