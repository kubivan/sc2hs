module DummyBot (DummyBot (..)) where

import Agent (Agent (..), StepPlan)
import SC2.Proto.Data qualified as Proto

data DummyBot = DummyBot

instance Agent DummyBot where
  makeAgent _ _ _ _ _ = pure DummyBot
  agentRace _ = Proto.Protoss
  agentStep bot _ _ = pure (bot, mempty :: StepPlan)
