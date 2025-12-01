{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Agent (
    Agent (..),
    StepPlan (..),
)
where

import Actions (Action, DebugCommand (..))
import SC2.Proto.Data qualified as Proto
import UnitAbilities

import Data.Text (Text)
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

    makeAgent :: a -> Word32 -> Proto.ResponseGameInfo -> Proto.ResponseData -> Proto.ResponseObservation -> IO a

    agentRace :: a -> Proto.Race
    agentStep :: a -> Proto.ResponseObservation -> UnitAbilities -> IO (a, StepPlan)