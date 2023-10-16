
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}


module Agent(Agent(..), Dummy(..), AgentLog(..), UnitAbilities, debug, command) where

import Actions qualified
import Data.String

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A

import Control.Monad.Writer.Strict

import qualified Data.HashMap.Strict as HashMap

import qualified AbilityId
import UnitTypeId


type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId.AbilityId]

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

class Agent s where
    race :: s -> C.Race
    --step :: (Agent a) => s -> A.Observation -> UnitAbilities -> Writer AgentLog s
    --step :: forall a. (Agent a) => s -> A.Observation -> UnitAbilities -> Writer AgentLog Agent s
    step :: s -> A.Observation -> UnitAbilities -> Writer AgentLog s

newtype Dummy = Dummy Integer
instance Agent Dummy where
    race _ = C.Protoss
    step (Dummy i) obs abs = do
        let msg = fromString $ "dummy hello " ++ show i -- ++ " " ++ show (playerInfo ^. #player_id) 
        command [Actions.Chat msg | i `mod` 100 == 0]
        return $ Dummy (i+1)