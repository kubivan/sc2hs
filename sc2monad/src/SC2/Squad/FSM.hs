{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module SC2.Squad.FSM where

import SC2.Squad.FSSquadForming
import SC2.Squad.FSSquadIdle
import SC2.Squad.FSExploreRegion
import SC2.Squad.FSEngage

-- import SC2.Army.Army
import SC2.Squad.Squad
import SC2.Squad.Class
import SC2.Squad.State
import SC2.Grid (RegionId)
import StepMonad
import Data.Maybe (isJust)


isSquadIdle :: FSMSquad SquadState -> Bool
isSquadIdle s = isJust (unwrapState @FSSquadIdle (squadState s))

squadAssignedRegion :: FSMSquad SquadState -> Maybe RegionId
squadAssignedRegion squad = case unwrapState @FSExploreRegion (squadState squad) of
  Just (FSExploreRegion rid _) -> Just rid
  Nothing -> Nothing

dispatchUpdate
  :: (HasArmy d, HasObs d, HasGrid d)
  => FSMSquad SquadState -> SquadState -> StepMonad d (Bool, SquadState)
dispatchUpdate squad (MkSquadState st) = do
  (done, st') <- fsUpdate squad st
  pure (done, wrapState st')

dispatchStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchStep squad (MkSquadState st) = fsStep squad st

dispatchOnEnter
  :: (HasArmy d, HasObs d, HasGrid d)
  => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnEnter squad (MkSquadState st) = fsOnEnter squad st

dispatchOnExit
  :: (HasArmy d, HasObs d, HasGrid d)
  => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnExit squad (MkSquadState st) = fsOnExit squad st

dispatchTransitionNext
  :: (HasArmy d, HasObs d, HasGrid d)
  => FSMSquad SquadState -> SquadState -> StepMonad d SquadState
dispatchTransitionNext squad (MkSquadState st) = fsTransitionNext squad st

type TransitionChooser d = FSMSquad SquadState -> SquadState -> StepMonad d SquadState

defaultTransitionChooser :: (HasArmy d, HasObs d, HasGrid d) => TransitionChooser d
defaultTransitionChooser = dispatchTransitionNext


processSquadWith ::(HasArmy d, HasObs d, HasGrid d) => TransitionChooser d -> FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquadWith chooseTransition squad = do
      (done, state') <- dispatchUpdate squad (squadState squad)
      if done
        then squadTransitionFromWith chooseTransition squad state'
        else do
          dispatchStep squad state'
          return squad { squadState = state' }

processSquad ::(HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquad = processSquadWith defaultTransitionChooser

squadTransitionFromWith :: (HasArmy d, HasObs d, HasGrid d) => TransitionChooser d -> FSMSquad SquadState -> SquadState -> StepMonad d (FSMSquad SquadState)
squadTransitionFromWith chooseTransition squad oldState = do
  dispatchOnExit squad oldState
  stNew <- chooseTransition squad oldState
  dispatchOnEnter squad stNew

  return squad { squadState = stNew }

squadTransitionFrom :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d (FSMSquad SquadState)
squadTransitionFrom = squadTransitionFromWith defaultTransitionChooser
