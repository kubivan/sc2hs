module Squad.Class (HasArmy (..)) where

import Actions (UnitTag)
import Data.HashMap.Strict (HashMap)
import Units (Unit (..))

class HasArmy d where
    getUnitMap :: d -> HashMap UnitTag Unit
