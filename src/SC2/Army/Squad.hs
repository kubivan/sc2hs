{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE GADTs, ConstraintKinds, TypeApplications #-}

module SC2.Army.Squad where

import Actions (Action (..), UnitTag)
import SC2.Grid.Algo
import SC2.Grid.TilePos
import SC2.Army.Class
import Units
import Utils
import Footprint
import StepMonad
import SC2.Geometry

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe
import System.Random (Random, StdGen, randomR)
import Lens.Micro
import Lens.Micro.Extras (view)
import Data.Char (isDigit)
import Data.Typeable

data AnyFS where
  --AnyFS :: SquadFS s => s -> AnyFS
  AnyFS :: (SquadFS s, Typeable s) => s -> AnyFS


class SquadFS s where
    --type Owner s

    fsStep :: (HasArmy d, AgentDynamicState d) => Squad -> s -> StepMonad d ()
    fsUpdate :: (HasArmy d, AgentDynamicState d) => Squad -> s -> StepMonad d (Bool, s)

    fsOnEnter :: (HasArmy d, AgentDynamicState d) => Squad -> s -> StepMonad d ()
    fsOnExit :: (HasArmy d, AgentDynamicState d) => Squad -> s -> StepMonad d ()

data Squad = Squad
  {
    squadUnits  :: [UnitTag]
  , squadState  :: AnyFS
  } --deriving (Eq, Show)

squadId :: Squad -> UnitTag
squadId s = head $ squadUnits s

data Target
    = TargetPos TilePos
    | TargetUnit UnitTag
    deriving (Eq, Show)

replaceSquad :: Squad -> [Squad] -> [Squad]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)
