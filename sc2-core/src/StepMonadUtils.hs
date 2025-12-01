{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module StepMonadUtils (
    findPlacementPointInRadiusSM,
    addMarkSM,
    removeMarkSM,
    debugUnit,
    debugUnitVec,
    siUnitData,
    siUnitRange,
    siUnitSightRange
)
where

import Actions (Action, DebugCommand (..), getCmd, getExecutors, Line)
import Agent
import SC2.Grid
import Observation
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (PlayerInfo, Point, ResponseGameInfo, UnitTypeData, Point2D)
import UnitAbilities
import Utils
import Units
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

import SC2.Geometry
import Data.ProtoLens (defMessage)
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Raw_Fields qualified as PR

import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras(view)
import Debug.Trace
import SC2.Proto.Data qualified as D
import Proto.S2clientprotocol.Data_Fields (sightRange)


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

--debugUnit :: AgentDynamicState d => Unit -> StepMonad d ()
debugUnit unit = do
    ds <- agentGet
    let unitPos :: Point
        unitPos = unit ^. PR.pos
        unitZ = unitPos ^. #z
        zLevel :: Point
        zLevel = defMessage & C.x .~ 0 & C.y .~ 0 & C.z .~ (1 + unitZ)
        p0 :: Point
        p0 = unit ^. PR.pos
        p1 :: Point
        p1 = zLevel + toPoint3D (toPoint2D p0 + unitVelocityVec unit)
        colorGreen = defMessage & #r .~ 0 & #g .~ 1 & #b .~ 0
        line :: Line
        --line = trace ("line from " ++ show (p0, p1)) $ defMessage & #p0 .~ p0 & #p1 .~ p1
        line = defMessage & #p0 .~ p0 & #p1 .~ p1
    StepMonad.debug [DebugLine [(colorGreen, line)] ]

debugUnitVec :: AgentDynamicState d => Unit -> Point2D -> StepMonad d ()
debugUnitVec unit vec2d = do
    ds <- agentGet
    let unitPos :: Point
        unitPos = unit ^. PR.pos
        unitZ = unitPos ^. #z
        zLevel :: Point
        zLevel = defMessage & C.x .~ 0 & C.y .~ 0 & C.z .~ (1 + unitZ)
        p0 :: Point
        p0 = unit ^. PR.pos
        p1 :: Point
        p1 = zLevel + toPoint3D (toPoint2D p0 + vec2d)
        colorGreen = defMessage & #r .~ 0 & #g .~ 255 & #b .~ 0
        line :: Line
        --line = trace ("line from " ++ show (p0, p1)) $ defMessage & #p0 .~ p0 & #p1 .~ p1
        line = defMessage & #p0 .~ p0 & #p1 .~ p1
        -- line = defMessage & #p0 .~ p0 & #p1 .~ p1
    StepMonad.debug [DebugLine [(colorGreen, line)] ]

siUnitData :: AgentDynamicState d => Unit -> StepMonad d UnitTypeData
siUnitData u = do
    si <- agentStatic
    let traits = unitTraits si
        udata = (HashMap.!) traits . toEnum' . view #unitType $ u
    return udata

headF :: String -> [a] -> a
headF err []    = error err
headF _   (x:_) = x

siUnitRange :: AgentDynamicState d => Unit -> Unit -> StepMonad d Float
siUnitRange u e = do -- TODO: take into account different data.weapons: air/ground etc
    udata <- siUnitData u
    let range = view #range $ headF ("no weapons in unit " ++ show u) $ udata ^. #weapons
    return range

siUnitSightRange :: AgentDynamicState d => Unit -> StepMonad d Float
siUnitSightRange u = do
    udata <- siUnitData u
    let range = view #sightRange udata
    return range
