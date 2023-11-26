
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Units
  ( unitsSelf,
    unitsNew,
    getUnit,
    unitsChanged,
    findNexus,
    enemyBaseLocation,
    obsResources,
    Cost (..),
    Unit,
    obsUnitsC,
    runC,
    unitTypeC,
    equalsC,
    allianceC,
    mapTilePosC,
    closestC,
    clusterUnits,
    isMineral,
    isGeyser,
    (.|),
    dbscan,
    PointLabel (..),
    MapClusters,
    toEnum',
    fromEnum',
  )
where

import Lens.Micro

import Data.Ord ()
import qualified Data.Map as Map
import Data.List (foldl', groupBy)
import Data.Function (on)

import GHC.Word (Word64, Word32)
import Conduit
import Utils

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A
import qualified Proto.S2clientprotocol.Raw as PR
import qualified Proto.S2clientprotocol.Raw_Fields as PR

import Agent ( Observation ) --TODO: HACK: move observation to a separate module, resolve labels conflict
import UnitTypeId
import Proto.S2clientprotocol.Common (Point)
import Conduit (Identity)
type Unit = PR.Unit

toEnum' :: Enum e => GHC.Word.Word32 -> e
toEnum' = toEnum . fromIntegral

fromEnum' :: Enum e => e -> GHC.Word.Word32
fromEnum' = fromIntegral . fromEnum

isMineral :: Unit -> Bool
isMineral u = utype == NeutralMineralfield 
  || utype == NeutralMineralfield750 
  || utype == NeutralLabmineralfield 
  || utype == NeutralLabmineralfield
  || utype == NeutralLabmineralfield750 
  || utype == NeutralRichmineralfield
  || utype == NeutralRichmineralfield750
  || utype == NeutralPurifierrichmineralfield750
  || utype == NeutralPurifierrichmineralfield
  || utype == NeutralBattlestationmineralfield
  || utype == NeutralBattlestationmineralfield750
    where
      utype = toEnum' $ u ^. #unitType

isGeyser :: Unit -> Bool
isGeyser u = utype == NeutralVespenegeyser
  || utype == NeutralRichvespenegeyser
  || utype == NeutralProtossvespenegeyser
  || utype == NeutralSpaceplatformgeyser
  || utype == NeutralPurifiervespenegeyser
  || utype == NeutralShakurasvespenegeyser
  where
    utype = toEnum' $ u ^. #unitType


unitsChanged :: Observation -> Observation -> Bool
unitsChanged obs obsPrev = ulen obs /= ulen obsPrev || firstStep where --`Utils.dbg` (show (obs ^. #gameLoop) ++ " " ++ (show (obsPrev ^. #gameLoop))) where
    ulen obs = length $ runC (unitsSelf obs)
    firstStep = (obs ^. #gameLoop) == 1 &&  (obsPrev ^. #gameLoop) == 0

unitsNew :: Observation -> Observation -> [Unit]
unitsNew obs obsPrev = filter notInPrev (runC $ unitsSelf obs) where
    notInPrev u = (u ^. #tag) `notElem` map (^. #tag) unitsPrev
    unitsPrev = runC $ unitsSelf obsPrev

getUnit :: Observation -> Word64 -> Unit
getUnit obs utag =
  head $ runC $ unitsSelf obs .| filterC (\ u -> u ^. #tag == utag) .| filterC (\u -> u ^. #tag == utag)

equalsC :: (Monad m, Eq a) => Getting a s a -> a -> ConduitT s s m ()
equalsC label value = filterC (\u -> u ^. label == value)

allianceC a = #alliance `equalsC` a

unitTypeC t = #unitType `equalsC` fromEnum' t

--unitsSelf :: Observation -> [Unit]
unitsSelf :: Observation -> ConduitT a Unit Identity ()
unitsSelf obs = obsUnitsC obs .| allianceC PR.Self

mapTilePosC :: Conduit Unit Identity TilePos
mapTilePosC = mapC (^. #pos) .| mapC tilePos

--runC :: a -> [Unit]
runC x = runConduitPure (x .| sinkList)

obsUnitsC :: Observation -> ConduitT i Unit Identity ()
obsUnitsC obs = yieldMany (obs ^. (#rawData . #units))

closestC :: (Monad m) => Unit -> ConduitT Unit Void m (Maybe Unit)
closestC to = await >>= foldlC (\mu u -> closest <$> mu <*> pure u)
  where
    toPos = to ^. #pos
    closest a b = if distSquared (a ^. #pos) toPos < distSquared (b ^. #pos) toPos then a else b

findNexus :: Observation -> PR.Unit
findNexus obs = head $ runC $ unitsSelf obs .| unitTypeC ProtossNexus

enemyBaseLocation :: A.ResponseGameInfo -> Observation -> Point2D
enemyBaseLocation gi obs = head $ filter notCloseToNexus enemyBases where
  nexus = findNexus obs
  notCloseToNexus p = distSquared p (to2D (nexus ^. #pos) ) > 1
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

distSquaredU :: Unit -> Unit -> Float
distSquaredU a b = distSquared (a ^. #pos) (b ^. #pos)

type ClusterId = Int
data PointLabel = Noise | Cluster ClusterId
  deriving (Show, Eq, Ord)

type MapClusters = Map.Map Unit PointLabel

rangeQuery :: Float -> Unit -> [Unit] -> [Unit]
rangeQuery eps p = filter (\q -> distSquaredU p q <= eps * eps)

expandCluster :: Float -> Unit -> ClusterId -> MapClusters -> [Unit] -> MapClusters
expandCluster eps point clusterId pointStatusMap points =
  foldl' expandCluster' pointStatusMap $ rangeQuery eps point points -- `Utils.dbg` ("expand cluster " ++ show clusterId)
    where
      expandCluster' :: MapClusters -> Unit -> MapClusters
      expandCluster' mapAcc q =
        case Map.lookup q mapAcc of -- `Utils.dbg` ("expandCluster' " ++ show clusterId ++ " p: " ++ show (q ^. #pos) ++ " count:" ++ show (countCluster mapAcc clusterId)) of
          Nothing -> expandCluster eps q clusterId (Map.insert q (Cluster clusterId) mapAcc) points
          Just v       -> mapAcc --`Utils.dbg` ("already visited: " ++ show v)

dbscan :: Float -> Int -> [Unit] -> MapClusters
dbscan eps minPts points = foldl' dbscan' Map.empty points --`Utils.dbg` ("points to dsbscan: " ++ show (length points))
  where
    --composeRes res (u, Cluster i) = Map.insert res i u

    dbscan' :: MapClusters -> Unit -> MapClusters
    dbscan' labels p =
      case Map.lookup p labels of --`Utils.dbg` ("dsbscan: process point" ++ show (p ^. #pos)) of
        Nothing -> let neighbors = rangeQuery eps p points
                       clusterId = Map.size labels
                       label    = if length neighbors >= minPts then Cluster clusterId else Noise
                       labels' = Map.insert p label labels --`Utils.dbg` ("label: " ++ show label ++ " neighbors: " ++ show (length neighbors))
                   in if label == Noise then labels' else expandCluster eps p clusterId labels' points -- `Utils.dbg` (" " ++ show p ++ " " ++ show clusterId ++ " " ++ show label)
        Just v       -> labels --`Utils.dbg` ("dsbscan: point is processed " ++ show v)

countCluster clusters n = Map.foldlWithKey (\count _ label -> if label == Cluster n then count + 1 else count) 0 clusters
