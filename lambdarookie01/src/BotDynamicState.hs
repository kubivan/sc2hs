{-# LANGUAGE ImportQualifiedPost #-}

module BotDynamicState where

import Actions (UnitTag)
import Army.Army
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap
import Intent (BuildIntentStore)
import Lens.Micro (Lens', (%~), (^.))
import Observation
import SC2.Grid
import Squad.Class
import StepMonad
import System.Random (Random, StdGen, randomR)

data BotDynamicState = BotDynamicState
    { dsObs :: Observation
    , dsGrid :: Grid
  , dsReservedCost :: Cost
    , dsRandGen :: StdGen
    , dsArmy :: Army
    , dsBuildIntents :: BuildIntentStore
    }

class HasBuildIntents d where
  buildIntentsL :: Lens' d BuildIntentStore

class HasReservedCost d where
  reservedCostL :: Lens' d Cost

instance HasObs BotDynamicState where
  obsL f s = f (dsObs s) <&> \o -> s { dsObs = o }

instance HasGrid BotDynamicState where
  gridL f s = f (dsGrid s) <&> \g -> s { dsGrid = g }

instance HasReservedCost BotDynamicState where
  reservedCostL f s = f (dsReservedCost s) <&> \c -> s { dsReservedCost = c }

instance HasBuildIntents BotDynamicState where
  buildIntentsL f s = f (dsBuildIntents s) <&> \intents -> s { dsBuildIntents = intents }

instance HasArmy BotDynamicState where
  getUnitMap bds = armyUnits $ dsArmy bds

agentGetBuildIntents :: (HasBuildIntents d) => StepMonad d BuildIntentStore
agentGetBuildIntents = agentGet <&> (^. buildIntentsL)

agentModifyBuildIntents :: (HasBuildIntents d) => (BuildIntentStore -> BuildIntentStore) -> StepMonad d ()
agentModifyBuildIntents f = agentModify (buildIntentsL %~ f)

agentGetReservedCost :: (HasReservedCost d) => StepMonad d Cost
agentGetReservedCost = agentGet <&> (^. reservedCostL)

agentModifyReservedCost :: (HasReservedCost d) => (Cost -> Cost) -> StepMonad d ()
agentModifyReservedCost f = agentModify (reservedCostL %~ f)

setRandGen :: StdGen -> BotDynamicState -> BotDynamicState
setRandGen gen (BotDynamicState obs grid reserved _ army intents) = BotDynamicState obs grid reserved gen army intents

getRandValue :: (Random a) => (a, a) -> BotDynamicState -> (a, BotDynamicState)
getRandValue range (BotDynamicState obs grid reserved gen army intents) =
    let (value, newGen) = randomR range gen
  in (value, BotDynamicState obs grid reserved newGen army intents)

bdsUpdateArmyUnitData :: BotDynamicState -> UnitTag -> ArmyUnitData -> BotDynamicState
bdsUpdateArmyUnitData ds tag newUnitData = ds{dsArmy = dsArmy'}
  where
    army = dsArmy ds
    dsArmy' = army{armyUnitsData = armyUnitsData'}
    armyUnitsData' = HashMap.insert tag newUnitData (armyUnitsData army)
