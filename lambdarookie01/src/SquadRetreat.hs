
module SquadRetreat where

import Squad.Squad
import Squad.State
import Squad.Class
import SC2.Geometry
import SC2.Grid
import StepMonad
import Actions (Action (PointCommand), UnitTag)
import SC2.Ids.AbilityId (AbilityId (ATTACKATTACK))

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes, listToMaybe)
import Lens.Micro ((^.))
import Debug.Trace (traceM)

-- ---------------------------------------------------------------------------
-- Step

retreatStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> TilePos -> StepMonad d ()
retreatStep squad rallyPos = do
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        units = catMaybes [unitByTag t | t <- squadUnits squad]
    if null units
        then pure ()
        else command [PointCommand ATTACKATTACK units (toPoint2D rallyPos)]

-- ---------------------------------------------------------------------------
-- Update

retreatUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> TilePos -> StepMonad d (Bool, SquadState)
retreatUpdate squad rallyPos = do
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        units = catMaybes [unitByTag t | t <- squadUnits squad]
        arrived = case listToMaybe units of
            Nothing     -> True
            Just leader -> distManhattan (tilePos (leader ^. #pos)) rallyPos <= 2
    pure (arrived, SSRetreat rallyPos)

-- ---------------------------------------------------------------------------
-- Enter / Exit / Transition

retreatOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
retreatOnEnter squad = traceM $ "[enter] SquadRetreat " ++ show (squadId squad)

retreatOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
retreatOnExit squad = traceM $ "[exit] SquadRetreat " ++ show (squadId squad)

retreatTransitionNext :: (HasArmy d) => FSMSquad SquadState -> StepMonad d SquadState
retreatTransitionNext _ = pure SSIdle

