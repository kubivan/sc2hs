{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module BotDynamicState where

import Actions (UnitTag)
import Agent
import Grid.Grid
import Observation
import UnitTypeId
import Units
import Utils (TilePos, tilePos)

import Data.HashMap.Strict qualified as HashMap
import Data.Set qualified as Set
import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras (view)
import System.Random (Random, StdGen, newStdGen, randomR)

data Target = TargetPos TilePos | TargetUnit UnitTag deriving (Eq, Show)

data ArmyUnitState = StateIdle | StateExplore Target | StateExploreRegion RegionId Region | StateAttack Target | StateEvade deriving (Eq, Show)

armyUnitStateStr :: ArmyUnitState -> String
armyUnitStateStr (StateIdle) = "Idle"
armyUnitStateStr (StateExplore t) = "Explore: " ++ show t
armyUnitStateStr (StateExploreRegion rid r) = "ExploreRegion: " ++ show rid ++ " size" ++ show (length r)
armyUnitStateStr (StateAttack t) = "Attack: " ++ show t
armyUnitStateStr (StateEvade) = "Evade"

data ProtossUnit = Stalker Unit ArmyUnitState | Zealot Unit ArmyUnitState

data ArmySquad = ArmySquad
    { squadUnits :: [UnitTag]
    , squadState :: ArmyUnitState
    }
    deriving (Eq, Show)

squadId :: ArmySquad -> UnitTag
squadId s = head $ squadUnits s

isSquadIdle :: ArmySquad -> Bool
isSquadIdle s = case squadState s of
    StateIdle -> True
    _ -> False

data ArmyUnitData = ArmyUnitData
    { auVisitedTiles :: Set.Set TilePos
    , auUnvisitedEdge :: Set.Set TilePos
    }

data Army = Army
    { armyUnitsData :: HashMap.HashMap UnitTag ArmyUnitData
    , armyUnits :: HashMap.HashMap UnitTag Unit
    , armyUnitsPos :: Set.Set TilePos
    , armySquads :: [ArmySquad]
    }

emptyArmy :: Army
emptyArmy = Army HashMap.empty HashMap.empty Set.empty []

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

bdsUpdateArmyUnitData :: BotDynamicState -> UnitTag -> ArmyUnitData -> BotDynamicState
bdsUpdateArmyUnitData ds tag newUnitData = ds{dsArmy = dsArmy'}
  where
    army = dsArmy ds
    dsArmy' = army{armyUnitsData = armyUnitsData'}
    armyUnitsData' = HashMap.insert tag newUnitData (armyUnitsData army)