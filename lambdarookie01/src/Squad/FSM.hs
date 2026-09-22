module Squad.FSM where

import Squad.FSEngage
import Squad.FSExploreRegion
import Squad.FSSquadForming
import Squad.FSSquadIdle
import SquadRetreat

import Army.Class (HasArmy)
import Control.Monad (void)
import SC2.Grid (RegionId)
import Squad.Squad
import Squad.State
import StepMonad
import StepMonadUtils (removeMarkSM)

isSquadIdle :: FSMSquad SquadState -> Bool
isSquadIdle s = case squadState s of
  SSIdle -> True
  _ -> False

squadAssignedRegion :: FSMSquad SquadState -> Maybe RegionId
squadAssignedRegion squad = case squadState squad of
  SSExploreRegion (FSExploreRegion rid _) -> Just rid
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- State updates and actions

updateState ::
  (HasArmy d, HasObs d, HasGrid d) =>
  FSMSquad SquadState -> SquadState -> StepMonad d SquadState
updateState squad SSIdle = idleUpdate squad
updateState squad (SSForming s) = formingUpdate squad s
updateState squad (SSExploreRegion s) = exploreRegionUpdate squad s
updateState squad (SSEngage s@(FSEngageFar _)) = engageFarUpdate squad s
updateState squad (SSEngage s@(FSEngageClose _)) = engageCloseUpdate squad s
updateState squad (SSRetreat s) = retreatUpdate squad s

stepState ::
  (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d ()
stepState squad SSIdle = idleStep squad
stepState squad (SSForming f) = formingStep squad f
stepState squad (SSExploreRegion s) = exploreRegionStep squad s
stepState squad (SSEngage s@(FSEngageFar _)) = engageFarStep squad s
stepState squad (SSEngage s@(FSEngageClose _)) = engageCloseStep squad s
stepState squad (SSRetreat s) = retreatStep squad s

-- | The only state-exit resource is a placed formation's grid mark.  Keep its
-- cleanup next to the state replacement so callers cannot bypass it.
setSquadState ::
  (HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d (FSMSquad SquadState)
setSquadState squad state' = do
  case (squadState squad, state') of
    (SSForming (FSFormingPlaced _), SSForming _) -> pure ()
    (SSForming (FSFormingPlaced (center, footprint)), _) -> void $ removeMarkSM footprint center
    _ -> pure ()
  pure squad{squadState = state'}

processSquad ::
  (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquad squad = do
  state' <- updateState squad (squadState squad)
  squad' <- setSquadState squad state'
  stepState squad' state'
  pure squad'
