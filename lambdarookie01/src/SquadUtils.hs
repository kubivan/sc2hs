module SquadUtils (squadUnits, debugSquad) where

import Army.Army (armyByTag)
import Army.Class (HasArmy)
import Data.Maybe (catMaybes, fromJust)
import Squad.Squad
import Squad.State (SquadState)
import StepMonad (StepMonad)
import StepMonadUtils (debugUnit)
import Units (Unit)

squadUnits :: (HasArmy d) => FSMSquad SquadState -> StepMonad d [Unit]
squadUnits squad = catMaybes <$> mapM armyByTag (squadTags squad)

debugSquad :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
debugSquad squad = do
  leader <- fromJust <$> armyByTag (head . squadTags $ squad)
  debugUnit leader
