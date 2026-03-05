
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module SC2.Squad.State where

import SC2.Squad.Squad
import Data.Typeable (Typeable, cast)

data SquadState = forall st. (Typeable st, SquadFS SquadState st) => MkSquadState st

wrapState :: (Typeable st, SquadFS SquadState st) => st -> SquadState
wrapState = MkSquadState

unwrapState :: forall st. Typeable st => SquadState -> Maybe st
unwrapState (MkSquadState st) = cast st