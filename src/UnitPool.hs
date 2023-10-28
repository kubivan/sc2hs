
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitPool(unitsSelf, unitsNew, getUnit, unitsChanged, findNexus, enemyBaseLocation, obsResources, Cost(..)) where

import Lens.Micro

-- import qualified Proto.S2clientprotocol.Sc2api as P
-- import qualified Proto.S2clientprotocol.Sc2api_Fields as P
-- 
-- import qualified Proto.S2clientprotocol.Common as PC
-- import qualified Proto.S2clientprotocol.Common_Fields as PC
-- import qualified Proto.S2clientprotocol.Raw as PR
-- import qualified Proto.S2clientprotocol.Raw_Fields as PR

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
type Unit = PR.Unit

unitsSelf :: Observation -> [Unit]
unitsSelf obs = filter (\u -> u ^. #alliance == PR.Self) (obs ^. (#rawData . #units))

unitsNew obs obsPrev = filter notInPrev (unitsSelf obs) where
    notInPrev u = (u ^. #tag) `notElem` map (^. #tag) unitsPrev
    unitsPrev = unitsSelf obsPrev

unitsChanged obs obsPrev = ulen obs /= ulen obsPrev || firstStep where --`Utils.dbg` (show (obs ^. #gameLoop) ++ " " ++ (show (obsPrev ^. #gameLoop))) where
    ulen = length . unitsSelf
    firstStep = (obs ^. #gameLoop) == 1 &&  (obsPrev ^. #gameLoop) == 0

getUnit :: Observation -> Word64 -> Unit
getUnit obs utag =
  head $
    runConduitPure $
      yieldMany (unitsSelf obs)
        .| filterC (\u -> u ^. #tag == utag)
        .| sinkList

protossNexus = 59 :: Word32 -- TODO: fixme

findNexus :: Observation -> PR.Unit
findNexus obs = head $ filter (\u -> (u ^. #unitType) == protossNexus) (unitsSelf obs) -- `Utils.debug` ("unitself " ++ (show unitsSelf))

enemyBaseLocation :: A.ResponseGameInfo -> Observation -> Point2D
enemyBaseLocation gi obs = head $ filter notCloseToNexus enemyBases where
  nexus = head $ filter (\u -> (u ^. #unitType) == protossNexus) (unitsSelf obs) -- `Utils.debug` ("unitself " ++ (show unitsSelf))
  notCloseToNexus p = distSquared p (to2D (nexus ^. #pos) ) > 1
  enemyBases = gi ^. (#startRaw . #startLocations)

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
