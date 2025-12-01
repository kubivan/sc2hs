module SC2.Squad(module FSM, Squad, module SC2.Squad.Squad, debugSquad) where


import SC2.Squad.FSM as FSM
import SC2.Squad.State
import SC2.Squad.Class
import SC2.Squad.Squad
import SC2.Squad.Behavior
import SC2.Squad.FSExploreRegion
import Units

import Data.HashMap.Strict as HashMap
import Data.Maybe

import StepMonad
import StepMonadUtils

type Squad = FSMSquad SquadState

debugSquad :: (HasArmy d, AgentDynamicState d) => Squad -> StepMonad d ()
debugSquad squad = do
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        leader = fromJust $ unitByTag $ head $ squadUnits squad
    debugUnit leader