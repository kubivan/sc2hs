{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module StepMonadUtils (
    findPlacementPointInRadiusSM,
    addMarkSM,
    removeMarkSM,
)
where

import Actions (Action, DebugCommand (..), getCmd, getExecutors)
import Agent
import SC2.Grid
import Observation
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (PlayerInfo, Point, ResponseGameInfo, UnitTypeData)
import UnitAbilities
import Utils
import StepMonad
import Footprint

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.Functor
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set qualified as Set
import Data.Text (pack)
import Lens.Micro ((^.))


-- findPlacementPointInRadius :: Grid -> Grid -> Footprint -> TilePos -> Float -> Maybe TilePos
findPlacementPointInRadiusSM :: AgentDynamicState d => Footprint -> TilePos -> Float -> StepMonad d (Maybe TilePos)
findPlacementPointInRadiusSM fprint start radius = do
    si <- agentStatic
    ds <- agentGet
    let obs = getObs ds
        grid = getGrid ds
    return $ findPlacementPointInRadius grid (heightMap si) fprint start radius

addMarkSM :: AgentDynamicState d => Footprint -> TilePos -> StepMonad d Grid
addMarkSM fprint cpos = do
    ds <- agentGet
    let obs = getObs ds
        grid = getGrid ds
    return $ addMark grid fprint cpos

removeMarkSM :: AgentDynamicState d => Footprint -> TilePos -> StepMonad d Grid
removeMarkSM fprint cpos = do
    ds <- agentGet
    let obs = getObs ds
        grid = getGrid ds
    return $ removeMark grid fprint cpos
