{-# LANGUAGE ImportQualifiedPost #-}

module BotDynamicState where

import Actions (UnitTag)
import SC2.Grid
import Observation
import Units
import StepMonad
import SC2.Army.Army
import SC2.Army.Class

import System.Random (Random, StdGen, randomR)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set(Set)
import Data.Set qualified as Set

data BotDynamicState = BotDynamicState
    { observation :: Observation
    , grid :: Grid
    , randGen :: StdGen
    , dsArmy :: Army
    }

-- Update the AgentDynamicState instance for AgentDynamicState
instance AgentDynamicState BotDynamicState where
    getObs (BotDynamicState obs _ _ _) = obs
    getGrid (BotDynamicState _ grid _ _) = grid

    setObs obs (BotDynamicState _ grid gen army) = BotDynamicState obs grid gen army
    setGrid grid (BotDynamicState obs _ gen army) = BotDynamicState obs grid gen army
    dsUpdate obs grid (BotDynamicState _ _ gen army) = BotDynamicState obs grid gen army

instance HasArmy BotDynamicState where
  --getArmy = dsArmy
  --getUnitMap :: d -> HashMap UnitTag Unit
  getUnitMap bds = armyUnits $ dsArmy bds
  --setArmy a st = st { dsArmy = a }

setRandGen :: StdGen -> BotDynamicState -> BotDynamicState
setRandGen gen (BotDynamicState obs grid _ army) = BotDynamicState obs grid gen army

-- Adding a function to retrieve random values from the dynamic state
getRandValue :: (Random a) => (a, a) -> BotDynamicState -> (a, BotDynamicState)
getRandValue range (BotDynamicState obs grid gen army) =
    let (value, newGen) = randomR range gen
     in (value, BotDynamicState obs grid newGen army)

bdsUpdateArmyUnitData :: BotDynamicState -> UnitTag -> ArmyUnitData -> BotDynamicState
bdsUpdateArmyUnitData ds tag newUnitData = ds{dsArmy = dsArmy'}
  where
    army = dsArmy ds
    dsArmy' = army{armyUnitsData = armyUnitsData'}
    armyUnitsData' = HashMap.insert tag newUnitData (armyUnitsData army)