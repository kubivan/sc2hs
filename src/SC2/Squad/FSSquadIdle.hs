
module SC2.Squad.FSSquadIdle where

import SC2.Squad.Class
import SC2.Squad.Types
import SC2.Utils
import SC2.Army.Class
import SC2.Squad.Squad
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

data FSSquadIdle = FSSquadIdle

instance SquadFS FSSquadIdle where

    fsStep s _ = traceM "[step] idle" -- >> wanderAround 5
    fsUpdate _ st = pure (True, st)
    fsOnEnter s _ = traceM $ "[enter] Idle " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] Idle " ++ show (squadId s)