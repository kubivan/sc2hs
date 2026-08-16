module Squad (module Squad.FSM, Squad, module Squad.Squad) where

import Squad.FSM
import Squad.Squad
import Squad.State

type Squad = FSMSquad SquadState
