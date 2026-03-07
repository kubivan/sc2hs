{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes, ConstraintKinds #-}
{-# LANGUAGE GADTs, ConstraintKinds #-}

module Squad.Squad(FSMSquad(..), squadId, SquadFS(..), Target(..), replaceSquad) where

import Actions (UnitTag)
import SC2.Grid.TilePos
import Squad.Class
import StepMonad


class SquadFS s st where

    fsStep :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad s -> st -> StepMonad d ()
    fsUpdate :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad s -> st -> StepMonad d (Bool, st)

    fsOnEnter :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad s -> st -> StepMonad d ()
    fsOnExit :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad s -> st -> StepMonad d ()
    fsTransitionNext :: (HasArmy d, HasObs d, HasGrid d) => FSMSquad s -> st -> StepMonad d s

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
