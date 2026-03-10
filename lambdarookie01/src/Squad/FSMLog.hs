module Squad.FSMLog (traceFSM, traceFSMWithState) where

import Debug.Trace (traceM)
import Squad.Squad
import Squad.State
import StepMonad

traceFSM :: FSMSquad SquadState -> String -> StepMonad d ()
traceFSM squad = traceFSMWithState squad (squadState squad)

traceFSMWithState :: FSMSquad SquadState -> SquadState -> String -> StepMonad d ()
traceFSMWithState squad state msg = traceM (prefix ++ " " ++ msg)
  where
    prefix = "[fsm][" ++ renderState state ++ "][" ++ renderSubstate state ++ "][" ++ show (squadId squad) ++ "]"

renderState :: SquadState -> String
renderState SSIdle = "idle"
renderState (SSForming _) = "forming"
renderState (SSExploreRegion _) = "explore-region"
renderState (SSEngage _) = "engage"
renderState (SSRetreat _) = "retreat"

renderSubstate :: SquadState -> String
renderSubstate SSIdle = "none"
renderSubstate (SSForming FSFormingUnplaced) = "unplaced"
renderSubstate (SSForming (FSFormingPlaced _)) = "placed"
renderSubstate (SSExploreRegion _) = "region"
renderSubstate (SSEngage (FSEngageFar _)) = "far"
renderSubstate (SSEngage (FSEngageClose _)) = "close"
renderSubstate (SSRetreat Nothing) = "pending-point"
renderSubstate (SSRetreat (Just _)) = "retreating"
