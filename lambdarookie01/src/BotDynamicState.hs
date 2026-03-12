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
    , dsRandGen :: StdGen
    , dsArmy :: Army
    , dsBuildIntents :: BuildIntentStore
    }

class HasBuildIntents d where
  buildIntentsL :: Lens' d BuildIntentStore

instance HasObs BotDynamicState where
  obsL f s = f (dsObs s) <&> \o -> s { dsObs = o }

instance HasGrid BotDynamicState where
  gridL f s = f (dsGrid s) <&> \g -> s { dsGrid = g }

instance HasBuildIntents BotDynamicState where
  buildIntentsL f s = f (dsBuildIntents s) <&> \intents -> s { dsBuildIntents = intents }

instance HasArmy BotDynamicState where
  getUnitMap bds = armyUnits $ dsArmy bds

agentGetBuildIntents :: (HasBuildIntents d) => StepMonad d BuildIntentStore
agentGetBuildIntents = agentGet <&> (^. buildIntentsL)

agentModifyBuildIntents :: (HasBuildIntents d) => (BuildIntentStore -> BuildIntentStore) -> StepMonad d ()
agentModifyBuildIntents f = agentModify (buildIntentsL %~ f)

setRandGen :: StdGen -> BotDynamicState -> BotDynamicState
setRandGen gen (BotDynamicState obs grid _ army intents) = BotDynamicState obs grid gen army intents

getRandValue :: (Random a) => (a, a) -> BotDynamicState -> (a, BotDynamicState)
getRandValue range (BotDynamicState obs grid gen army intents) =
    let (value, newGen) = randomR range gen
     in (value, BotDynamicState obs grid newGen army intents)

bdsUpdateArmyUnitData :: BotDynamicState -> UnitTag -> ArmyUnitData -> BotDynamicState
bdsUpdateArmyUnitData ds tag newUnitData = ds{dsArmy = dsArmy'}
  where
    army = dsArmy ds
    dsArmy' = army{armyUnitsData = armyUnitsData'}
    armyUnitsData' = HashMap.insert tag newUnitData (armyUnitsData army)
