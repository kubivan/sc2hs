{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Agent (
    Agent (..),
    StepPlan (..),
)
where

import AbilityId qualified
import Actions (Action, DebugCommand (..), getCmd, getExecutors)
import Control.Monad
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.Functor
import Data.HashMap.Strict qualified as HashMap
import Data.List (foldl')
import Data.Map qualified as Map
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Debug.Trace
import GHC.Word qualified
import Grid.Grid
import Lens.Micro ((&), (.~), (^.))
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Data qualified as A
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields qualified as A
import UnitTypeId

import Utils

import Data.Kind (Type)
import Observation
import UnitAbilities
import Data.Word (Word32)

data StepPlan = StepPlan
    { botCommands :: [Action]
    , botChat :: [Text]
    , botDebug :: [DebugCommand]
    }

instance Semigroup StepPlan where
    (<>) (StepPlan as1 cs1 ds1) (StepPlan as2 cs2 ds2) = StepPlan (as1 <> as2) (cs1 <> cs2) (ds1 <> ds2)

instance Monoid StepPlan where
    mempty = StepPlan [] [] []

class Agent a where

    makeAgent :: a -> Word32 -> A.ResponseGameInfo -> A.ResponseData -> A.ResponseObservation -> IO a

    agentRace :: a -> C.Race
    agentStep :: a -> A.ResponseObservation -> UnitAbilities -> (a, StepPlan)