
module Squad.FSSquadIdle where

import Squad.Class
import Squad.FSMLog
import Squad.Squad
import Squad.State
import StepMonad

idleStep :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
idleStep squad = traceFSM squad "step"

idleUpdate :: (HasArmy d) => FSMSquad SquadState -> StepMonad d UpdateResult
idleUpdate _ = pure (Continue SSIdle)

idleOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
idleOnEnter squad = traceFSM squad "enter"

idleOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
idleOnExit squad = traceFSM squad "exit"
