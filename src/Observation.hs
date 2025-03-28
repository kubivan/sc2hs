
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Observation
  ( Observation,
    addOrder,
    addUnit,
    gridUpdate,
    unitsSelf,
    unitsNew,
    getUnit,
    buildingsSelfChanged,
    findNexus,
    enemyBaseLocation,
    obsResources,
    Cost (..),
    obsUnitsC,
    clusterUnits,
    findExpands
  )
where

import Utils
import Grid
import Footprint
import Units
import UnitTypeId
import AbilityId
import Actions(UnitTag)

import GHC.Word (Word64, Word32)

import qualified Data.Map as Map
import Data.ProtoLens
import Data.Maybe (isJust, catMaybes, mapMaybe)
import Data.List (foldl', maximumBy, minimumBy, groupBy, sortOn)
import Data.Function (on)

import Proto.S2clientprotocol.Data qualified as A
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields qualified as A

import qualified Proto.S2clientprotocol.Raw as PR
import qualified Proto.S2clientprotocol.Raw_Fields as PR
import Proto.S2clientprotocol.Common (Point, Point2D)
import Lens.Micro
import Lens.Micro.Extras(view)
import Conduit
import Proto.S2clientprotocol.Raw_Fields (buildProgress)

import Safe (headMay)

type Observation = A.Observation

addOrder :: UnitTag -> AbilityId -> Observation -> Observation
addOrder unitTag ability obs=
  obs & #rawData . A.units . traverse . filtered (\unit -> unit ^. #tag == unitTag) . #orders %~ (order :) --TODO: append order to the end?
  where
    order = defMessage & #abilityId .~ fromEnum' ability & #progress .~ -1 -- TODO: add target & progress

addUnit :: UnitTypeId -> Observation -> Observation
addUnit unitType obs =
  obs & #rawData . A.units %~ (unit unitType :)
  where
    unit :: UnitTypeId -> Units.Unit
    unit t = defMessage & #unitType .~ fromEnum' t & #buildProgress .~ -1 -- TODO: add target & progress

gridUpdate :: Observation -> Grid.Grid -> Grid.Grid
gridUpdate obs grid = foldl (\acc (fp, pos) -> Grid.addMark acc fp pos) grid (getFootprints <$> units)
  where -- `Utils.dbg` ("gridUpdate" ++ show fp ++ " " ++ show pos)) grid (getFootprints <$> units)
    --units = filter (\u -> toEnum' (u ^. #unitType) /= ProtossProbe) (obs ^. (#rawData . #units))
    units = runConduitPure $ obsUnitsC obs .| filterC (\u -> isBuilding u || isMineral u) .| sinkList
    getFootprints :: Units.Unit -> (Footprint, TilePos)
    getFootprints u = (getFootprint (toEnum' $ u ^. #unitType), tilePos $ u ^. #pos) -- `Utils.dbg` ("getFootPrint " ++ show (toEnum' (u ^. #unitType) :: UnitTypeId) ++ " " ++ show (tilePos $ u ^. #pos))

buildingsSelfChanged :: Observation -> Observation -> Bool
buildingsSelfChanged obs obsPrev = blen obs /= blen obsPrev || blen obs == 1 where --`Utils.dbg` (show (obs ^. #gameLoop) ++ " " ++ (show (obsPrev ^. #gameLoop))) where
    blen obs = length . runC $ unitsSelf obs .| filterC isBuilding .| filterC (\u -> u ^. #buildProgress > 0)

unitsNew :: Observation -> Observation -> [Unit]
unitsNew obs obsPrev = filter notInPrev (runC $ unitsSelf obs) where
    notInPrev u = (u ^. #tag) `notElem` map (^. #tag) unitsPrev
    unitsPrev = runC $ unitsSelf obsPrev

getUnit :: Observation -> UnitTag -> Maybe Unit
getUnit obs utag =
  headMay $ runC $ obsUnitsC obs .| filterC (\ u -> u ^. #tag == utag) .| filterC (\u -> u ^. #tag == utag)

unitsSelf :: Observation -> ConduitT a Unit Identity ()
unitsSelf obs = obsUnitsC obs .| allianceC PR.Self

--TODO: it's redundant it's fixed on the grid level by adding '.' around resources
findExpandPosInCluster :: Grid -> Grid -> [Units.Unit] -> Maybe TilePos
findExpandPosInCluster grid heightMap cluster = gridBfs grid (tilePos . view #pos . head $ cluster) canPlaceDist69 (const False)
  where
    clusterTiles = tilePos . view #pos <$> cluster
    canPlaceDist69 p = all (\c -> distSquared p c >= 6 * 6 && distSquared p c < 9 * 9) clusterTiles
      && canPlaceBuilding grid heightMap p (getFootprint ProtossNexus)

findExpands :: Observation -> Grid -> Grid -> [TilePos]
findExpands obs grid heights = mapMaybe (findExpandPosInCluster grid heights) clusteredUnits
  where
    resourceFields = runC $ obsUnitsC obs .| filterC (\x -> isMineral x || isGeyser x)
    marked = dbscan 10 2 resourceFields
    clusters = groupBy ((==) `on` snd) $ sortOn snd $ Map.toList marked
    clusteredUnits = map (map fst) clusters

obsUnitsC :: Observation -> ConduitT i Unit Identity ()
obsUnitsC obs = yieldMany (obs ^. (#rawData . #units))

findNexus :: Observation -> PR.Unit
findNexus obs = head $ runC $ unitsSelf obs .| unitTypeC ProtossNexus

enemyBaseLocation :: A.ResponseGameInfo -> Observation -> Point2D
enemyBaseLocation gi obs = head $ filter notCloseToNexus enemyBases where
  nexus = findNexus obs
  notCloseToNexus p = distSquared p (nexus ^. #pos)  > 1
  enemyBases = gi ^. (#startRaw . #startLocations)

-- TODO: move to a separate file
data Cost = Cost {mineralCost :: Int, gasCost :: Int}
  deriving (Show, Eq, Ord)

instance Num Cost where
  a + b = Cost (mineralCost a + mineralCost b) (gasCost a + gasCost b)
  a - b = Cost (mineralCost a - mineralCost b) (gasCost a - gasCost b)
  a * b = Cost (mineralCost a * mineralCost b) (gasCost a * gasCost b)
  negate (Cost mc gc) = Cost (-mc) (-gc)
  abs (Cost mc gc) = Cost (abs mc) (abs gc)
  signum (Cost mc gc) = Cost (signum mc) (signum gc)
  fromInteger n = Cost (fromInteger n) (fromInteger n)

obsResources :: Observation -> Cost
obsResources obs  = Cost minerals vespene where
    minerals = fromIntegral $ obs ^. (#playerCommon . #minerals) -- `Utils.debug` ("minerals: " ++ show minerals)
    vespene = fromIntegral $ obs ^. (#playerCommon . #vespene)

--dbscanUnits :: Enum p => Observation -> p -> [[(Unit, PointLabel)]]
clusterUnits :: Observation -> UnitTypeId -> [[Unit]]
clusterUnits obs uid = map fst <$> groupBy ((==) `on` snd) (Map.toList $ dbscan 10.0 1 units)
  where
    units = runC $ obsUnitsC obs .| unitTypeC uid