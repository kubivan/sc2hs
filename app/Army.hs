
module Army where

import Actions (Action (..), UnitTag)
import Grid.Algo
import Units
import Utils

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (Random, StdGen, randomR)

data Target
    = TargetPos TilePos
    | TargetUnit UnitTag
    deriving (Eq, Show)

data SquadState
    = StateIdle
    | StateExplore Target
    | StateExploreRegion RegionId Region
    | StateAttack Target
    | StateEvade
    deriving (Eq, Show)

armyUnitStateStr :: SquadState -> String
armyUnitStateStr (StateIdle) = "Idle"
armyUnitStateStr (StateExplore t) = "Explore: " ++ show t
armyUnitStateStr (StateExploreRegion rid r) = "ExploreRegion: " ++ show rid ++ " size" ++ show (length r)
armyUnitStateStr (StateAttack t) = "Attack: " ++ show t
armyUnitStateStr (StateEvade) = "Evade"

-- data ProtossUnit = Stalker Unit ArmyUnitState | Zealot Unit ArmyUnitState

data ArmySquad = ArmySquad
    { squadUnits :: [UnitTag]
    , squadState :: SquadState
    , squadUnitStates :: HashMap UnitTag SquadState
    }
    deriving (Eq, Show)

squadId :: ArmySquad -> UnitTag
squadId s = head $ squadUnits s

isSquadIdle :: ArmySquad -> Bool
isSquadIdle s = case squadState s of
    StateIdle -> True
    _ -> False

data ArmyUnitData = ArmyUnitData
    { auVisitedTiles :: Set TilePos
    , auUnvisitedEdge :: Set TilePos
    }

data Army = Army
    { armyUnitsData :: HashMap UnitTag ArmyUnitData
    , armyUnits :: HashMap UnitTag Unit
    , armyUnitsPos :: Set TilePos
    , armySquads :: [ArmySquad]
    }

emptyArmy :: Army
emptyArmy = Army HashMap.empty HashMap.empty Set.empty []