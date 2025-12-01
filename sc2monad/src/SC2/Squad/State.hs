
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes #-}

module SC2.Squad.State where

import SC2.Squad.Class
import SC2.Squad.FSExploreRegion
import SC2.Squad.FSEngage
import SC2.Squad.FSSquadForming
import SC2.Squad.FSSquadIdle


data SquadState where
  SquadIdleState      :: FSSquadIdle         -> SquadState
  SquadFormingState   :: FSSquadForming      -> SquadState
  SquadExploreState   :: FSExploreRegion     -> SquadState
  SquadEngageEnemy   :: FSEngage     -> SquadState

class IsSquadFS st where
  wrapState :: st -> SquadState
  unwrapState :: SquadState -> Maybe st

instance IsSquadFS FSEngage where
    wrapState = SquadEngageEnemy
    unwrapState (SquadEngageEnemy st) = Just st
    unwrapState _ = Nothing

instance IsSquadFS FSExploreRegion where
    wrapState = SquadExploreState
    unwrapState (SquadExploreState st) = Just st
    unwrapState _ = Nothing

instance IsSquadFS FSSquadForming where
    wrapState = SquadFormingState
    unwrapState (SquadFormingState st) = Just st
    unwrapState _ = Nothing
    --unwrapState _ =

instance IsSquadFS FSSquadIdle where
    wrapState = SquadIdleState
    unwrapState (SquadIdleState st) = Just st
    unwrapState _ = Nothing