{-# LANGUAGE MultiParamTypeClasses #-}

module SquadRetreat where

import SC2.Squad.Squad
import SC2.Squad.State
import SC2.Squad.FSSquadIdle (FSSquadIdle (..))
import SC2.Squad.Class
import SC2.Geometry
import SC2.Grid
import StepMonad
import Actions (Action (PointCommand), UnitTag)
import SC2.Ids.AbilityId (AbilityId (ATTACKATTACK))

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes, listToMaybe)
import Lens.Micro ((^.))
import Debug.Trace (traceM)

data SquadRetreat = SquadRetreat TilePos

instance SquadFS SquadState SquadRetreat where
    fsStep squad (SquadRetreat rallyPos) = do
        ds <- agentGet
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            units = catMaybes [unitByTag t | t <- squadUnits squad]
        if null units
            then pure ()
            else command [PointCommand ATTACKATTACK units (toPoint2D rallyPos)]

    fsUpdate squad st@(SquadRetreat rallyPos) = do
        ds <- agentGet
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            units = catMaybes [unitByTag t | t <- squadUnits squad]
            arrived = case listToMaybe units of
                Nothing -> True
                Just leader -> distManhattan (tilePos (leader ^. #pos)) rallyPos <= 2
        pure (arrived, st)

    fsOnEnter squad _ = traceM $ "[enter] SquadRetreat " ++ show (squadId squad)
    fsOnExit squad _ = traceM $ "[exit] SquadRetreat " ++ show (squadId squad)
    fsTransitionNext _ _ = pure $ wrapState FSSquadIdle
