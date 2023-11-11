
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
    closestC
  )
where

import Lens.Micro

import Data.Ord(comparing)

import GHC.Word (Word64)
import Conduit
import Utils

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A
import qualified Proto.S2clientprotocol.Raw as PR
import qualified Proto.S2clientprotocol.Raw_Fields as PR

import Agent ( Observation, fromEnum' ) --TODO: HACK: move observation to a separate module, resolve labels conflict
import UnitTypeId
import Proto.S2clientprotocol.Common (Point)
import Conduit (Identity)
type Unit = PR.Unit

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
closestC to = await >>= process
  where
    process elem@(Just e) = fmap Just $ foldlC (getClosest to) e

getClosest :: Unit -> Unit -> Unit -> Unit
getClosest to a b  = if distSquared (to2D $ a ^. #pos) toPos < distSquared (to2D $ b ^. #pos) toPos then a else b
  where 
    toPos = to2D $ to ^. #pos

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
