module SC2.Army.Army where

import Actions (Action (..), UnitTag)
import SC2.Grid.Algo
import SC2.Grid.TilePos
import Units
import Utils
import Footprint
import SC2.Army.Squad
import SC2.Army.SquadFSM
import StepMonad

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe
import System.Random (Random, StdGen, randomR)

-- class HasArmy d where
--   getArmy :: d -> Army
--   setArmy :: Army -> d -> d

data Target
    = TargetPos TilePos
    | TargetUnit UnitTag
    deriving (Eq, Show)

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


