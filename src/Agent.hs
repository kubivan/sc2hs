
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}


module Agent(Agent(..), AgentLog(..), StaticInfo(..), UnitAbilities, UnitTraits, Observation, debug, command) where

import Actions qualified

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A

import Control.Monad.Writer.Strict

import qualified Data.HashMap.Strict as HashMap

import qualified AbilityId
import UnitTypeId
import qualified Proto.S2clientprotocol.Data as A

type Observation = A.Observation

type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId.AbilityId]

type UnitTraits = HashMap.HashMap UnitTypeId A.UnitTypeData

data AgentLog = AgentLog
  {
    botCommands :: [Actions.Action]
  , botDebug :: [Actions.DebugCommand]
  }

command :: [Actions.Action] -> Writer AgentLog ()
command acts = tell (AgentLog acts [])

debug :: [Actions.DebugCommand] -> Writer AgentLog ()
debug acts = tell (AgentLog [] acts)

instance Semigroup AgentLog where
  (<>) (AgentLog as1 ds1) (AgentLog as2 ds2) = AgentLog (as1 <> as2) (ds1 <> ds2)

instance Monoid AgentLog where
    mempty = AgentLog [] []

data StaticInfo = StaticInfo { gameInfo :: A.ResponseGameInfo, playerInfo :: A.PlayerInfo, unitTraits :: UnitTraits}

class Agent a where
    agentRace :: a -> C.Race
    agentStep :: a -> StaticInfo -> A.Observation -> UnitAbilities -> Writer AgentLog a
    agentDebug :: a -> IO ()