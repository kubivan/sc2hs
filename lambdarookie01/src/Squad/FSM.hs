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
    SSIdle -> True
    _      -> False

squadAssignedRegion :: FSMSquad SquadState -> Maybe RegionId
squadAssignedRegion squad = case squadState squad of
    SSExploreRegion rid _ -> Just rid
    _                     -> Nothing

-- ---------------------------------------------------------------------------
-- Dispatch

dispatchUpdate
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d (Bool, SquadState)
dispatchUpdate squad SSIdle                      = idleUpdate squad
dispatchUpdate squad (SSForming f)               = formingUpdate squad f
dispatchUpdate squad (SSExploreRegion rid region) = exploreRegionUpdate squad rid region
dispatchUpdate squad (SSEngageFar tag)           = engageFarUpdate squad tag
dispatchUpdate squad (SSEngageClose tag)         = engageCloseUpdate squad tag
dispatchUpdate squad (SSRetreat pos)             = retreatUpdate squad pos

dispatchStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchStep squad SSIdle                       = idleStep squad
dispatchStep squad (SSForming f)                = formingStep squad f
dispatchStep squad (SSExploreRegion _ region)   = exploreRegionStep squad region
dispatchStep squad (SSEngageFar tag)            = engageFarStep squad tag
dispatchStep squad (SSEngageClose tag)          = engageCloseStep squad tag
dispatchStep squad (SSRetreat pos)              = retreatStep squad pos

dispatchOnEnter
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnEnter squad SSIdle          = idleOnEnter squad
dispatchOnEnter squad (SSForming f)   = formingOnEnter squad f
dispatchOnEnter squad st@(SSExploreRegion{}) = exploreRegionOnEnter squad
dispatchOnEnter squad (SSEngageFar _) = engageOnEnter squad
dispatchOnEnter squad (SSEngageClose _) = engageOnEnter squad
dispatchOnEnter squad (SSRetreat _)   = retreatOnEnter squad

dispatchOnExit
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnExit squad SSIdle            = idleOnExit squad
dispatchOnExit squad (SSForming f)     = formingOnExit squad f
dispatchOnExit squad st@(SSExploreRegion{}) = exploreRegionOnExit squad
dispatchOnExit squad (SSEngageFar _)   = engageOnExit squad
dispatchOnExit squad (SSEngageClose _) = engageOnExit squad
dispatchOnExit squad (SSRetreat _)     = retreatOnExit squad

dispatchTransitionNext
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d SquadState
dispatchTransitionNext squad SSIdle             = pure SSIdle
dispatchTransitionNext squad (SSForming _)      = formingTransitionNext squad
dispatchTransitionNext squad (SSExploreRegion{}) = exploreRegionTransitionNext squad
dispatchTransitionNext squad (SSEngageFar _)    = engageTransitionNext squad
dispatchTransitionNext squad (SSEngageClose _)  = engageTransitionNext squad
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

