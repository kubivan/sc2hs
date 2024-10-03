
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Units
  ( Unit,
    UnitOrder,
    runC,
    unitTypeC,
    unitIdleC,
    equalsC,
    allianceC,
    mapTilePosC,
    closestC,
    isMineral,
    isGeyser,
    isAssimilator,
    (.|),
    dbscan,
    PointLabel (..),
    MapClusters,
    toEnum',
    fromEnum',
    unitsBoundingBox,
  )
where

import Lens.Micro

import Data.Ord ()
import qualified Data.Map as Map
import Data.List (foldl', maximumBy, minimumBy, groupBy, sortOn)
import Data.Function (on)

import GHC.Word (Word64, Word32)
import Conduit
import Utils
import Lens.Micro.Extras(view)

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A
import qualified Proto.S2clientprotocol.Raw as PR
import qualified Proto.S2clientprotocol.Raw_Fields as PR

import UnitTypeId
import Proto.S2clientprotocol.Common (Point)
import Conduit (Identity)
import Grid (Grid, canPlaceBuilding, gridBfs)
import Footprint (getFootprint)
import Data.Maybe (isJust, catMaybes, mapMaybe)
type Unit = PR.Unit
type UnitOrder = PR.UnitOrder

toEnum' :: Enum e => GHC.Word.Word32 -> e
toEnum' = toEnum . fromIntegral

fromEnum' :: Enum e => e -> GHC.Word.Word32
fromEnum' = fromIntegral . fromEnum

isMineral :: PR.Unit -> Bool
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
      utype = toEnum' $ u ^. PR.unitType

isGeyser :: PR.Unit -> Bool
isGeyser u = utype == NeutralVespenegeyser
  || utype == NeutralRichvespenegeyser
  || utype == NeutralProtossvespenegeyser
  || utype == NeutralSpaceplatformgeyser
  || utype == NeutralPurifiervespenegeyser
  || utype == NeutralShakurasvespenegeyser
  where
    utype = toEnum' $ u ^. PR.unitType

isAssimilator :: PR.Unit -> Bool
isAssimilator u = utype == ProtossAssimilator
  || utype == ProtossAssimilatorrich
  where
    utype = toEnum' $ u ^. PR.unitType

equalsC :: (Monad m, Eq a) => Getting a s a -> a -> ConduitT s s m ()
equalsC label value = filterC (\u -> u ^. label == value)

allianceC a = #alliance `equalsC` a

unitIdleC :: (Monad m) => ConduitT Unit Unit m ()
unitIdleC = filterC (Prelude.null . view PR.orders)

unitTypeC t = #unitType `equalsC` fromEnum' t

mapTilePosC :: Conduit Unit Identity TilePos
mapTilePosC = mapC (^. PR.pos) .| mapC tilePos

--runC :: a -> [Unit]
runC x = runConduitPure (x .| sinkList)

closestC :: (Monad m) => Unit -> ConduitT Unit Void m (Maybe Unit)
closestC to = await >>= foldlC (\mu u -> closest <$> mu <*> pure u)
  where
    toPos = to ^. PR.pos
    closest a b = if distSquared (a ^. PR.pos) toPos < distSquared (b ^. PR.pos) toPos then a else b

unitsBoundingBox :: [Unit] -> (TilePos, TilePos)
unitsBoundingBox cluster = ((tileX minX, tileY minY), (tileX maxX, tileY maxY))
  where
    minX = minimumBy (compare `on` view C.x) points
    maxX = maximumBy (compare `on` view C.x) points

    minY = minimumBy (compare `on` view C.y) points
    maxY = maximumBy (compare `on` view C.y) points

    points = view PR.pos <$> cluster


distSquaredU :: Unit -> Unit -> Float
distSquaredU a b = distSquared (a ^. PR.pos) (b ^. PR.pos)

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