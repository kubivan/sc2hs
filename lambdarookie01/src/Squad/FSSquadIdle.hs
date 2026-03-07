
module Squad.FSSquadIdle where

import Squad.Class
import Squad.Squad
import Squad.State
import StepMonad

import Debug.Trace (traceM)

idleStep :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
idleStep _ = traceM "[step] idle"

idleUpdate :: (HasArmy d) => FSMSquad SquadState -> StepMonad d (Bool, SquadState)
idleUpdate _ = pure (False, SSIdle)

idleOnEnter :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
idleOnEnter s = traceM $ "[enter] Idle " ++ show (squadId s)

idleOnExit :: (HasArmy d) => FSMSquad SquadState -> StepMonad d ()
idleOnExit s = traceM $ "[exit] Idle " ++ show (squadId s)
