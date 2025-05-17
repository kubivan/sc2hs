module SC2.Squad(module FSM, Squad, module SC2.Squad.Squad) where


import SC2.Squad.FSM as FSM
import SC2.Squad.State
import SC2.Squad.Squad
import SC2.Squad.Behavior
import SC2.Squad.FSExploreRegion

type Squad = FSMSquad SquadState