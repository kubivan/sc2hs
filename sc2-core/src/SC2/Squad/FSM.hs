{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes #-}

module SC2.Squad.FSM where

import SC2.Squad.FSSquadForming
import SC2.Squad.FSSquadIdle
import SC2.Squad.FSExploreRegion

-- import SC2.Army.Army
import SC2.Utils
import SC2.Squad.Squad hiding (SquadState(..))
import SC2.Squad.Class
import SC2.Squad.State
import SC2.Grid
import SC2.Geometry
import StepMonad
import StepMonadUtils
import Actions (Action (..), UnitTag)
import SC2.Ids.AbilityId

import Control.Monad (void)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Set qualified as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

import Debug.Trace (traceM)
import Footprint

import Data.Char (isDigit)

data SomeFS where
  SomeFS :: (SquadFS st, IsSquadFS st) => st -> SomeFS

isSquadIdle :: FSMSquad SquadState -> Bool
isSquadIdle s = case unwrapState (squadState s) of
    Just (FSSquadIdle) -> True
    Nothing -> False

squadAssignedRegion :: FSMSquad SquadState -> Maybe RegionId
squadAssignedRegion squad = case unwrapState(squadState squad) of
    Just (FSExploreRegion rid _) -> Just rid
    Nothing -> Nothing

dispatchUpdate
  :: (HasArmy d, AgentDynamicState d)
  => FSMSquad SquadState -> SquadState -> StepMonad d (Bool, SquadState)
dispatchUpdate squad state = case matchState state of
  Just (SomeFS st) -> do
    (done, newSt) <- fsUpdate squad st
    pure (done, wrapState newSt)
  Nothing -> error "Unknown SquadState in dispatchUpdate"

dispatchStep :: (HasArmy d, AgentDynamicState d) => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchStep squad state = case matchState state of
  Just (SomeFS st) -> fsStep squad st
  Nothing -> error "Unknown SquadState in dispatchStep"

dispatchOnEnter
  :: forall d. (HasArmy d, AgentDynamicState d)
  => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnEnter squad st = case matchState st of
  Just (SomeFS st) -> fsOnEnter squad st
  Nothing -> error "Unknown SquadState in dispatchStep"

dispatchOnExit
  :: forall d. (HasArmy d, AgentDynamicState d)
  => FSMSquad SquadState -> SquadState -> StepMonad d ()
dispatchOnExit squad st = case matchState st of
  Just (SomeFS st) -> fsOnExit squad st
  Nothing -> error "Unknown SquadState in dispatchStep"


matchState :: SquadState -> Maybe SomeFS
matchState (SquadIdleState st)    = Just (SomeFS st)
matchState (SquadFormingState st) = Just (SomeFS st)
matchState (SquadExploreState st) = Just (SomeFS st)
matchState (SquadEngageEnemy st) = Just (SomeFS st)


processSquad ::(HasArmy d, AgentDynamicState d) => FSMSquad SquadState -> StepMonad d (FSMSquad SquadState)
processSquad squad = do
      (done, state') <- dispatchUpdate squad (squadState squad)
      if done
        then squadTransitionFrom squad state'
        else do
          dispatchStep squad state'
          return squad { squadState = state' }

squadTransitionFrom :: (HasArmy d, AgentDynamicState d) => FSMSquad SquadState -> SquadState -> StepMonad d (FSMSquad SquadState)
squadTransitionFrom squad oldState = do
  dispatchOnExit squad oldState

  -- Pick new state (e.g., Idle)
  let stNew = wrapState FSSquadIdle  -- or whatever your decision logic is
  dispatchOnEnter squad stNew

  return squad { squadState = stNew }
