
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module TestBot(TestBot(..), ProbeRush(..)) where

import Data.String

import Agent
import qualified Actions
import qualified Utils

import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C

import Lens.Micro ( (&), (.~), (&), (.~), (^.) )

import Utils
import Proto.S2clientprotocol.Debug_Fields (unitTag)

import qualified AbilityId
import UnitTypeId

data ProbeRush = ProbeRush {probeRushRace :: C.Race}

instance Agent ProbeRush where
    agentRace = probeRushRace 
    agentStep a gameInfo _ obs abilities stepCount = do
        let enemyBase = Utils.enemyBaseLocation gameInfo obs
        command [Actions.UnitCommand AbilityId.Attack (head (Utils.unitsSelf obs) ^. #tag) enemyBase ]
        return a 

data TestBot = Opening | EmptyBot

instance Agent TestBot where
    agentRace _ = C.Protoss
    agentStep e@EmptyBot _ _ _ _ _ = return e
    --step :: s -> A.ResponseGameInfo -> A.PlayerInfo -> A.Observation -> UnitAbilities -> Int -> Writer AgentLog s
    agentStep a gameInfo playerInfo obs abilities stepCount = do
        let msg = fromString $ "test hello " ++ (show $ gameInfo ^. #mapName) -- ++ show i -- ++ " " ++ show (playerInfo ^. #player_id) 
        let enemyBase = Utils.enemyBaseLocation gameInfo obs
        if stepCount `mod` 10 == 0 then command [Actions.Chat msg, Actions.Chat (fromString.show $ enemyBase) ] else command []
        debug [Actions.DebugText "testU" (c ^. #pos) | c <- Utils.unitsSelf obs ]
        --tell [Actions.UnitCommand AbilityId.Attack (head (Utils.unitsSelf obs) ^. #tag) enemyBase ]
        --tell [Actions.Chat msg]
        return a