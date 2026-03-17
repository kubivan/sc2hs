{-# LANGUAGE ImportQualifiedPost #-}

module BotDynamicState where

import Actions (UnitTag)
import Army.Army
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap
import Intent (HasBuildIntents (..), IntentStore)
import Lens.Micro ((%~), (^.))
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
    , dsIntents :: IntentStore BotDynamicState
    }

instance HasObs BotDynamicState where
    obsL f s = f (dsObs s) <&> \o -> s{dsObs = o}

instance HasGrid BotDynamicState where
    gridL f s = f (dsGrid s) <&> \g -> s{dsGrid = g}

instance HasReservedCost BotDynamicState where
    reservedCostL f s = f (dsReservedCost s) <&> \c -> s{dsReservedCost = c}

instance HasBuildIntents BotDynamicState where
    buildIntentsL f s = f (dsIntents s) <&> \intents -> s{dsIntents = intents}

instance HasArmy BotDynamicState where
    getUnitMap bds = armyUnits $ dsArmy bds

agentGetBuildIntents :: (HasBuildIntents d) => StepMonad d (IntentStore d)
agentGetBuildIntents = agentGet <&> (^. buildIntentsL)

agentModifyBuildIntents :: (HasBuildIntents d) => (IntentStore d -> IntentStore d) -> StepMonad d ()
agentModifyBuildIntents f = agentModify (buildIntentsL %~ f)

agentModifyArmy :: (Army -> Army) -> StepMonad BotDynamicState ()
agentModifyArmy f =
    agentModify $ \ds -> ds{dsArmy = f (dsArmy ds)}

agentPutArmyUnitData :: UnitTag -> ArmyUnitData -> StepMonad BotDynamicState ()
agentPutArmyUnitData tag newUnitData =
    agentModify $ \ds -> bdsUpdateArmyUnitData ds tag newUnitData

agentWithRandGen :: (StdGen -> (a, StdGen)) -> StepMonad BotDynamicState a
agentWithRandGen f = do
    ds <- agentGet
    let (value, newGen) = f (dsRandGen ds)
    agentPut $ ds{dsRandGen = newGen}
    pure value

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
