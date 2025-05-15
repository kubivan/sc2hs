module SC2.Army.Class (HasArmy(..)) where

import SC2.Grid
import Units(Unit(..))
import Actions(UnitTag)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap


class HasGrid d where
  getGrid :: d -> Grid
  setGrid :: Grid -> d -> d

class HasArmy d where
  getUnitMap :: d -> HashMap UnitTag Unit