{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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




-- data Squad = Squad
--   {
--     squadUnits  :: [UnitTag]
--   , squadState  :: AnyFS
--   } --deriving (Eq, Show)
