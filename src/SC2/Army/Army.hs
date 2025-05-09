
module SC2.Army.Army where

import Actions (Action (..), UnitTag)
import SC2.Grid.Algo
import SC2.Grid.TilePos
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
    = StateSquadIdle
    | StateExplore Target
    | StateExploreRegion RegionId Region
    | StateAttack Target
    | StateEvade
    deriving (Eq, Show)

armyUnitStateStr :: SquadState -> String
armyUnitStateStr (StateSquadIdle) = "Idle"
armyUnitStateStr (StateExplore t) = "Explore: " ++ show t
armyUnitStateStr (StateExploreRegion rid r) = "ExploreRegion: " ++ show rid ++ " size" ++ show (length r)
armyUnitStateStr (StateAttack t) = "Attack: " ++ show t
armyUnitStateStr (StateEvade) = "Evade"

data ArmyUnitState
    = StateUnitIdle
    | StateUnitAttack
    | StateUnitMove
    | StateUnitEvade
    deriving (Eq, Show)

-- data ProtossUnit = Stalker Unit ArmyUnitState | Zealot Unit ArmyUnitState

data ArmySquad = ArmySquad
    { squadUnits :: [UnitTag]
    , squadState :: SquadState
    --, squadUnitStates :: HashMap UnitTag SquadState
    }
    deriving (Eq, Show)

squadId :: ArmySquad -> UnitTag
squadId s = head $ squadUnits s

isSquadIdle :: ArmySquad -> Bool
isSquadIdle s = case squadState s of
    StateSquadIdle -> True
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

replaceSquad :: ArmySquad -> [ArmySquad] -> [ArmySquad]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)