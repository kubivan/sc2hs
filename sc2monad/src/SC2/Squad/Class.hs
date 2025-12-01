
module SC2.Squad.Class(HasArmy(..)) where

import Data.HashMap.Strict(HashMap)
import Actions(UnitTag)
import Units(Unit(..))

class HasArmy d where
  getUnitMap :: d -> HashMap UnitTag Unit
