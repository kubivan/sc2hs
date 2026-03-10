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


isSquadIdle :: FSMSquad SquadState -> Bool
isSquadIdle s = case squadState s of
    SSIdle -> True
    _      -> False

squadAssignedRegion :: FSMSquad SquadState -> Maybe RegionId
squadAssignedRegion squad = case squadState squad of
    SSExploreRegion (FSExploreRegion rid _) -> Just rid
    _                                       -> Nothing

-- ---------------------------------------------------------------------------
-- Dispatch

dispatchUpdate
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d UpdateResult
dispatchUpdate squad SSIdle = idleUpdate squad
dispatchUpdate squad (SSForming s) = formingUpdate squad s
dispatchUpdate squad (SSExploreRegion s) = exploreRegionUpdate squad s
dispatchUpdate squad (SSEngage s@(FSEngageFar _)) = engageFarUpdate squad s
dispatchUpdate squad (SSEngage s@(FSEngageClose _)) = engageCloseUpdate squad s
dispatchUpdate squad (SSRetreat s) = retreatUpdate squad s

dispatchStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchStep squad SSIdle           = idleStep squad
dispatchStep squad (SSForming f)       = formingStep squad f
dispatchStep squad (SSExploreRegion s) = exploreRegionStep squad s
dispatchStep squad (SSEngage s@(FSEngageFar _))   = engageFarStep squad s
dispatchStep squad (SSEngage s@(FSEngageClose _)) = engageCloseStep squad s
dispatchStep squad (SSRetreat s)       = retreatStep squad s

dispatchOnEnter
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnEnter squad SSIdle              = idleOnEnter squad
dispatchOnEnter squad (SSForming _)       = formingOnEnter squad
dispatchOnEnter squad (SSExploreRegion _) = exploreRegionOnEnter squad
dispatchOnEnter squad (SSEngage _)        = engageOnEnter squad
dispatchOnEnter squad (SSRetreat _)       = retreatOnEnter squad

dispatchOnExit
    :: (HasArmy d, HasObs d, HasGrid d)
    => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnExit squad SSIdle              = idleOnExit squad
dispatchOnExit squad (SSForming _)       = formingOnExit squad
dispatchOnExit squad (SSExploreRegion _) = exploreRegionOnExit squad
dispatchOnExit squad (SSEngage _)        = engageOnExit squad
dispatchOnExit squad (SSRetreat _)       = retreatOnExit squad

processSquad :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquad squad = do
    result <- dispatchUpdate squad (squadState squad)
    case result of
        Continue state' -> do
            let squad' = squad { squadState = state' }
            dispatchStep squad' state'
            return squad'
        Transition stNew -> do
            dispatchOnExit squad (squadState squad)
            let squad' = squad { squadState = stNew }
            dispatchOnEnter squad' stNew
            processSquad squad'
            -- dispatchStep squad stNew
            -- return squad { squadState = stNew }
