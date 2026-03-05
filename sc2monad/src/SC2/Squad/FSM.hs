{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module SC2.Squad.FSM where

import SC2.Squad.FSSquadForming
import SC2.Squad.FSSquadIdle
import SC2.Squad.FSExploreRegion
import SC2.Squad.Behavior (isSquadFull)

-- import SC2.Army.Army
import SC2.Squad.Squad
import SC2.Squad.Class
import SC2.Squad.State
import SC2.Grid (RegionId)
import StepMonad


isSquadIdle :: FSMSquad SquadState -> Bool
isSquadIdle s = case squadState s of
  SquadIdleState FSSquadIdle -> True
  _ -> False

squadAssignedRegion :: FSMSquad SquadState -> Maybe RegionId
squadAssignedRegion squad = case squadState squad of
  SquadExploreState (FSExploreRegion rid _) -> Just rid
  _ -> Nothing

dispatchUpdate
  :: (HasArmy d, HasObs d, HasGrid d)
  => FSMSquad SquadState -> SquadState -> StepMonad d (Bool, SquadState)
dispatchUpdate squad (SquadIdleState st) = do
  (done, st') <- fsUpdate squad st
  pure (done, SquadIdleState st')
dispatchUpdate squad (SquadFormingState st) = do
  (done, st') <- fsUpdate squad st
  pure (done, SquadFormingState st')
dispatchUpdate squad (SquadExploreState st) = do
  (done, st') <- fsUpdate squad st
  pure (done, SquadExploreState st')
dispatchUpdate squad (SquadEngageEnemy st) = do
  (done, st') <- fsUpdate squad st
  pure (done, SquadEngageEnemy st')

dispatchStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchStep squad (SquadIdleState st) = fsStep squad st
dispatchStep squad (SquadFormingState st) = fsStep squad st
dispatchStep squad (SquadExploreState st) = fsStep squad st
dispatchStep squad (SquadEngageEnemy st) = fsStep squad st

dispatchOnEnter
  :: (HasArmy d, HasObs d, HasGrid d)
  => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnEnter squad (SquadIdleState st) = fsOnEnter squad st
dispatchOnEnter squad (SquadFormingState st) = fsOnEnter squad st
dispatchOnEnter squad (SquadExploreState st) = fsOnEnter squad st
dispatchOnEnter squad (SquadEngageEnemy st) = fsOnEnter squad st

dispatchOnExit
  :: (HasArmy d, HasObs d, HasGrid d)
  => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnExit squad (SquadIdleState st) = fsOnExit squad st
dispatchOnExit squad (SquadFormingState st) = fsOnExit squad st
dispatchOnExit squad (SquadExploreState st) = fsOnExit squad st
dispatchOnExit squad (SquadEngageEnemy st) = fsOnExit squad st

chooseNext :: (HasArmy d) => FSMSquad SquadState -> SquadState -> StepMonad d SquadState
chooseNext _ (SquadIdleState _) = pure $ SquadIdleState FSSquadIdle
chooseNext _ (SquadFormingState _) = pure $ SquadIdleState FSSquadIdle
chooseNext _ (SquadExploreState _) = pure $ SquadIdleState FSSquadIdle
chooseNext squad (SquadEngageEnemy _) = do
  full <- isSquadFull squad
  pure $ if full then SquadIdleState FSSquadIdle else SquadFormingState (FSSquadForming Nothing)


processSquad ::(HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquad squad = do
      (done, state') <- dispatchUpdate squad (squadState squad)
      if done
        then squadTransitionFrom squad state'
        else do
          dispatchStep squad state'
          return squad { squadState = state' }

squadTransitionFrom :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad SquadState -> SquadState -> StepMonad d (FSMSquad SquadState)
squadTransitionFrom squad oldState = do
  dispatchOnExit squad oldState
  stNew <- chooseNext squad oldState
  dispatchOnEnter squad stNew

  return squad { squadState = stNew }
