module Squad(module FSM, Squad, module Squad.Squad, debugSquad) where


import Squad.FSM as FSM
import Squad.State
import Squad.Class
import Squad.Squad
import Squad.Behavior
import Squad.FSExploreRegion
import Units

import Data.HashMap.Strict as HashMap
import Data.Maybe

import StepMonad
import StepMonadUtils

type Squad = FSMSquad SquadState

debugSquad :: (HasArmy d) => Squad -> StepMonad d ()
debugSquad squad = do
    ds <- agentGet
    let unitByTag t = HashMap.lookup t (getUnitMap ds)
        leader = fromJust $ unitByTag $ head $ squadUnits squad
    debugUnit leader
