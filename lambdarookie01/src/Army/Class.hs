module Army.Class where

import Actions (UnitTag)
import Squad.Squad
import Squad.State (SquadState)
import Units (Unit (..))

import Data.HashMap.Strict qualified as HashMap

class HasArmy d where
  getUnitMap :: d -> HashMap.HashMap UnitTag Unit

  getSquads :: d -> [FSMSquad SquadState]
