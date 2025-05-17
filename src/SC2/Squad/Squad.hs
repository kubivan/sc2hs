{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes, ConstraintKinds #-}
{-# LANGUAGE GADTs, ConstraintKinds #-}

module SC2.Squad.Squad(FSMSquad(..), squadId, SquadFS(..), Target(..), replaceSquad) where

import Actions (UnitTag)
import SC2.Grid.TilePos
import SC2.Squad.Class
import StepMonad


class SquadFS st where

    fsStep :: (HasArmy d, AgentDynamicState d) => FSMSquad a-> st -> StepMonad d ()
    fsUpdate :: (HasArmy d, AgentDynamicState d) => FSMSquad a -> st -> StepMonad d (Bool, st)

    fsOnEnter :: (HasArmy d, AgentDynamicState d) => FSMSquad a -> st -> StepMonad d ()
    fsOnExit :: (HasArmy d, AgentDynamicState d) => FSMSquad a -> st -> StepMonad d ()

data FSMSquad s = Squad
    { squadUnits :: [UnitTag]
    , squadState :: s -- SquadState
    }

squadId :: FSMSquad s -> UnitTag
squadId s = head $ squadUnits s

data Target
    = TargetPos TilePos
    | TargetUnit UnitTag
    deriving (Eq, Show)

replaceSquad :: FSMSquad s -> [FSMSquad s] -> [FSMSquad s]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)