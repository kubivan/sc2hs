module Army.Army where

import Actions (Action (..), UnitTag)
import Army.Class
import SC2.TilePos

import Squad.Squad
import StepMonad
import Units

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set
import Squad.State (SquadState)

-- import System.Random (Random, StdGen, randomR)

data ArmyUnitData = ArmyUnitData
  { auVisitedTiles :: Set TilePos
  , auUnvisitedEdge :: Set TilePos
  }

data Army = Army
  { armyUnitsData :: HashMap UnitTag ArmyUnitData
  , armyUnits :: HashMap UnitTag Unit
  , armyUnitsPos :: Set TilePos
  , armySquads :: [FSMSquad SquadState]
  }

armyByTag :: (HasArmy d) => UnitTag -> StepMonad d (Maybe Unit)
armyByTag t = do
  ds <- agentGet
  let units = getUnitMap ds
  pure $ HashMap.lookup t units

emptyArmy :: Army
emptyArmy = Army HashMap.empty HashMap.empty Set.empty []
