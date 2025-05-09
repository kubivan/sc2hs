module SC2.Army.Class (HasArmy(..)) where

import SC2.Army.Army
import SC2.Grid

class HasArmy d where
  getArmy :: d -> Army
  setArmy :: Army -> d -> d

class HasGrid d where
  getGrid :: d -> Grid
  setGrid :: Grid -> d -> d