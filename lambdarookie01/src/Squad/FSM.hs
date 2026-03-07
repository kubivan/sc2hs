module Squad.FSM where

import Squad.Class
import Squad.FSExploreRegion
import Squad.FSEngage
import Squad.FSSquadIdle
import Squad.FSSquadForming
import SquadRetreat

import Squad.Squad
import Squad.State
import SC2.Grid (RegionId)
import StepMonad
import Data.Maybe (isJust)


isSquadIdle :: FSMSquad SquadState -> Bool
isSquadIdle s = case squadState s of
    SSIdle _ -> True
    _        -> False

squadAssignedRegion :: FSMSquad SquadState -> Maybe RegionId
squadAssignedRegion squad = case squadState squad of
    SSExploreRegion (FSExploreRegion rid _) -> Just rid
    _                                       -> Nothing

-- ---------------------------------------------------------------------------
-- Dispatch

dispatchUpdate
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d (Bool, SquadState)
dispatchUpdate squad (SSIdle s) = do
    (done, s') <- idleUpdate squad
    pure (done, SSIdle s')
dispatchUpdate squad (SSForming s) = do
    (done, s') <- formingUpdate squad s
    pure (done, SSForming s')
dispatchUpdate squad (SSExploreRegion s) = do
    (done, s') <- exploreRegionUpdate squad s
    pure (done, SSExploreRegion s')
dispatchUpdate squad (SSEngage s@(FSEngageFar _)) = do
    (done, s') <- engageFarUpdate squad s
    pure (done, SSEngage s')
dispatchUpdate squad (SSEngage s@(FSEngageClose _)) = do
    (done, s') <- engageCloseUpdate squad s
    pure (done, SSEngage s')
dispatchUpdate squad (SSRetreat s) = do
    (done, s') <- retreatUpdate squad s
    pure (done, SSRetreat s')

dispatchStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchStep squad (SSIdle _)          = idleStep squad
dispatchStep squad (SSForming f)       = formingStep squad f
dispatchStep squad (SSExploreRegion s) = exploreRegionStep squad s
dispatchStep squad (SSEngage s@(FSEngageFar _))   = engageFarStep squad s
dispatchStep squad (SSEngage s@(FSEngageClose _)) = engageCloseStep squad s
dispatchStep squad (SSRetreat s)       = retreatStep squad s

dispatchOnEnter
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnEnter squad (SSIdle _)          = idleOnEnter squad
dispatchOnEnter squad (SSForming f)       = formingOnEnter squad f
dispatchOnEnter squad (SSExploreRegion _) = exploreRegionOnEnter squad
dispatchOnEnter squad (SSEngage _)        = engageOnEnter squad
dispatchOnEnter squad (SSRetreat _)       = retreatOnEnter squad

dispatchOnExit
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnExit squad (SSIdle _)          = idleOnExit squad
dispatchOnExit squad (SSForming f)       = formingOnExit squad f
dispatchOnExit squad (SSExploreRegion _) = exploreRegionOnExit squad
dispatchOnExit squad (SSEngage _)        = engageOnExit squad
dispatchOnExit squad (SSRetreat _)       = retreatOnExit squad

dispatchTransitionNext
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d SquadState
dispatchTransitionNext _ (SSIdle s)         = pure (SSIdle s)
dispatchTransitionNext squad (SSForming _)      = formingTransitionNext squad
dispatchTransitionNext squad (SSExploreRegion _) = exploreRegionTransitionNext squad
dispatchTransitionNext squad (SSEngage _)    = engageTransitionNext squad
dispatchTransitionNext squad (SSRetreat _)      = retreatTransitionNext squad

-- ---------------------------------------------------------------------------
-- Processing

type TransitionChooser d = FSMSquad SquadState -> SquadState -> StepMonad d SquadState

defaultTransitionChooser :: (HasArmy d, HasObs d, HasGrid d) => TransitionChooser d
defaultTransitionChooser = dispatchTransitionNext

processSquadWith :: (HasArmy d, HasObs d, HasGrid d) => TransitionChooser d -> FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquadWith chooseTransition squad = do
    (done, state') <- dispatchUpdate squad (squadState squad)
    if done
        then squadTransitionFromWith chooseTransition squad state'
        else do
            dispatchStep squad state'
            return squad { squadState = state' }

processSquad :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquad = processSquadWith defaultTransitionChooser

squadTransitionFromWith :: (HasArmy d, HasObs d, HasGrid d) => TransitionChooser d -> FSMSquad SquadState -> SquadState -> StepMonad d (FSMSquad SquadState)
squadTransitionFromWith chooseTransition squad oldState = do
    dispatchOnExit squad oldState
    stNew <- chooseTransition squad oldState
    dispatchOnEnter squad stNew
    return squad { squadState = stNew }

squadTransitionFrom :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d (FSMSquad SquadState)
squadTransitionFrom = squadTransitionFromWith defaultTransitionChooser

