
module SC2.Squad.Class(SquadFS(..), HasArmy(..)) where

import SC2.Squad.Types

import StepMonad
import Data.HashMap.Strict(HashMap)
import Actions(UnitTag)
import Units(Unit(..))

class HasArmy d where
  getUnitMap :: d -> HashMap UnitTag Unit

class SquadFS st where

    fsStep :: (HasArmy d, AgentDynamicState d) => Squad a-> st -> StepMonad d ()
    fsUpdate :: (HasArmy d, AgentDynamicState d) => Squad a -> st -> StepMonad d (Bool, st)

    fsOnEnter :: (HasArmy d, AgentDynamicState d) => Squad a -> st -> StepMonad d ()
    fsOnExit :: (HasArmy d, AgentDynamicState d) => Squad a -> st -> StepMonad d ()