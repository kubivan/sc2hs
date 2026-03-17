module Army.Army where

import Actions (Action (..), UnitTag)
import Footprint
import SC2.Grid.Algo
import SC2.Grid.TilePos
import Squad
import StepMonad
import Units
import Utils

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (Random, StdGen, randomR)

data ArmyUnitData = ArmyUnitData
    { auVisitedTiles :: Set TilePos
    , auUnvisitedEdge :: Set TilePos
    }

data Army = Army
    { armyUnitsData :: HashMap UnitTag ArmyUnitData
    , armyUnits :: HashMap UnitTag Unit
    , armyUnitsPos :: Set TilePos
    , armySquads :: [Squad]
    }

emptyArmy :: Army
emptyArmy = Army HashMap.empty HashMap.empty Set.empty []
