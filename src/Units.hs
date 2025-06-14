{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Units (
    Unit,
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
    isBuildingType,
    isBuilding,
    (.|),
    dbscan,
    PointLabel (..),
    MapClusters,
    toEnum',
    fromEnum',
    unitsBoundingBox,
    unitVelocityVec,
)
where

import Data.Maybe (isJust)
import SC2.Geometry
import SC2.Grid.TilePos
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Alliance (..), Point2D, Unit, UnitOrder)
import UnitsTh

import Conduit
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.Map qualified as Map
import Data.Ord ()
import Data.ProtoLens (Message (defMessage), defMessage)
import GHC.Word (Word32)
import Lens.Micro
import Lens.Micro.Extras (view)
import Utils

import Proto.S2clientprotocol.Common_Fields as C

-- import qualified Proto.S2clientprotocol.Raw as PR
import Proto.S2clientprotocol.Raw_Fields qualified as PR

toEnum' :: (Enum e) => GHC.Word.Word32 -> e
toEnum' = toEnum . fromIntegral

fromEnum' :: (Enum e) => e -> GHC.Word.Word32
fromEnum' = fromIntegral . fromEnum

-- TODO: check and remove if not needed

-- $(genBuildingMapping ''UnitTypeId ''AbilityId)

-- | TODO: remove compatibility wrapper
isBuildingType :: UnitTypeId -> Bool
isBuildingType = isUnitStructure

isBuilding :: Unit -> Bool
isBuilding = isBuildingType . toEnum' . view PR.unitType

isMineral :: Unit -> Bool
isMineral u =
    utype == NeutralMineralField
        || utype == NeutralMineralField750
         -- TODO: for some reason this check doesn't work: finds 36 patches from 148
        || (u ^. #mineralContents > 0 && u ^. #alliance == Neutral)
        -- TODO: add missing minerals
  where
    -- \|| utype == NeutralMineralField750
    -- \|| utype == NeutralLabMineralField
    -- \|| utype == NeutralLabMineralField
    -- \|| utype == NeutralLabMineralField750
    -- \|| utype == NeutralRichMineralField
    -- \|| utype == NeutralRichMineralField750
    -- \|| utype == NeutralPurifierrichMineralField750
    -- \|| utype == NeutralPurifierrichMineralField
    -- \|| utype == NeutralBattlestationMineralField
    -- \|| utype == NeutralBattlestationMineralField750

    utype = toEnum' $ u ^. PR.unitType

isGeyser :: Unit -> Bool
isGeyser u =
    utype == NeutralVespeneGeyser
        || utype == NeutralRichVespeneGeyser
        || hasGas
  where
    -- \|| utype == NeutralProtossvespenegeyser
    -- \|| utype == NeutralSpaceplatformGeyser
    -- \|| utype == NeutralPurifierVespeneGeyser
    -- \|| utype == NeutralShakurasVespeneGeyser

    utype = toEnum' $ u ^. PR.unitType
    hasGas = u ^. #vespeneContents > 0

isAssimilator :: Unit -> Bool
isAssimilator u =
    utype == ProtossAssimilator
        || utype == ProtossAssimilatorRich
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

-- runC :: a -> [Unit]
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
            Just v -> mapAcc -- `Utils.dbg` ("already visited: " ++ show v)

dbscan :: Float -> Int -> [Unit] -> MapClusters
dbscan eps minPts points = foldl' dbscan' Map.empty points -- `Utils.dbg` ("points to dsbscan: " ++ show (length points))
  where
    -- composeRes res (u, Cluster i) = Map.insert res i u

    dbscan' :: MapClusters -> Unit -> MapClusters
    dbscan' labels p =
        case Map.lookup p labels of -- `Utils.dbg` ("dsbscan: process point" ++ show (p ^. #pos)) of
            Nothing ->
                let neighbors = rangeQuery eps p points
                    clusterId = Map.size labels
                    label = if length neighbors >= minPts then Cluster clusterId else Noise
                    labels' = Map.insert p label labels -- `Utils.dbg` ("label: " ++ show label ++ " neighbors: " ++ show (length neighbors))
                 in if label == Noise then labels' else expandCluster eps p clusterId labels' points -- `Utils.dbg` (" " ++ show p ++ " " ++ show clusterId ++ " " ++ show label)
            Just v -> labels -- `Utils.dbg` ("dsbscan: point is processed " ++ show v)

countCluster clusters n = Map.foldlWithKey (\count _ label -> if label == Cluster n then count + 1 else count) 0 clusters

unitVelocityVec :: Unit -> Point2D
unitVelocityVec unit =
    let rotation = unit ^. #facing
        speed = 4.13 -- TODO: remove hardcoded stalkers value
        vx = speed * cos rotation
        vy = speed * sin rotation
     in defMessage & x .~ vx & y .~ vy
