module Squad.FSSquadIdle where

import Army.Class
import Squad.FSMLog
import Squad.Squad
import Squad.State
import StepMonad

idleStep :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
idleStep squad = traceFSM squad "step"

idleUpdate :: (HasArmy d) => FSMSquad SquadState -> StepMonad d SquadState
idleUpdate _ = pure SSIdle
