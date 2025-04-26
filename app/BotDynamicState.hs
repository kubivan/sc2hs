{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module BotDynamicState where

import Agent
import Observation
import Grid.Grid
import Utils(TilePos, tilePos)
import Units
import Actions(UnitTag)
import UnitTypeId

import Data.Set qualified as Set
import Data.HashMap.Strict qualified as HashMap
import System.Random (Random, StdGen, newStdGen, randomR)
import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras (view)
import Conduit (filterC, findC, headC, lengthC, mapC, runConduitPure, (.|))

import Proto.S2clientprotocol.Raw qualified as R

data Target = TargetPos TilePos | TargetUnit UnitTag

data ArmyUnitState = Exploring Target | Attacking Target | Evading

data ProtossUnit = Stalker Unit ArmyUnitState | Zealot Unit ArmyUnitState

data ArmySquad = ArmySquad
    { squadUnits :: [ProtossUnit]
    , squadState :: ArmyUnitState
    }

data ArmyUnitData = ArmyUnitData
    { auVisitedTiles :: Set.Set TilePos
    , auUnvisitedEdge :: Set.Set TilePos
    }

data Army = Army
    { armyUnits :: HashMap.HashMap UnitTag ArmyUnitData
    , armyUnitsPos :: Set.Set TilePos
    , armySquads :: [ArmySquad]
    }

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

setRandGen :: StdGen -> BotDynamicState -> BotDynamicState
setRandGen gen (BotDynamicState obs grid _ army) = BotDynamicState obs grid gen army

-- Adding a function to retrieve random values from the dynamic state
getRandValue :: (Random a) => (a, a) -> BotDynamicState -> (a, BotDynamicState)
getRandValue range (BotDynamicState obs grid gen army) =
    let (value, newGen) = randomR range gen
     in (value, BotDynamicState obs grid newGen army)

emptyArmy :: Army
emptyArmy = Army HashMap.empty Set.empty []

bdsUpdateArmyUnitData :: BotDynamicState -> UnitTag -> ArmyUnitData -> BotDynamicState
bdsUpdateArmyUnitData ds tag newUnitData= ds{dsArmy = dsArmy'}
    where
        army = dsArmy ds
        dsArmy' = army{armyUnits = armyUnits'}
        armyUnits' = HashMap.insert tag newUnitData (armyUnits army)

agentUpdateArmyPositions :: StepMonad BotDynamicState ()
agentUpdateArmyPositions = do
    ds <- agentGet
    obs <- agentObs -- allUnitPos = Set.fromList $ runC $ obsUnitsC obs .| filterC (not.isBuilding) .| mapC (view #pos) .| mapC tilePos
    let armyPoss = runC $ unitsSelf obs .| filterC isArmyUnit .| mapC (tilePos . view #pos)
        army' = (dsArmy ds){armyUnitsPos = Set.fromList armyPoss}

    agentPut $ ds{dsArmy = army'}

-- Define if a unit is considered an army unit
isArmyUnit :: Unit -> Bool -- TODO: remove protoss specific consts
isArmyUnit u = ProtossProbe /= utype && (not . isBuildingType $ utype)
  where
    utype = toEnum' (u ^. #unitType)

-- Define an enemy unit filter
isEnemy :: Unit -> Bool
isEnemy u = (u ^. #alliance) == R.Enemy -- Enemy alliance code