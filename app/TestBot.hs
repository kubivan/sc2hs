
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module TestBot(TestBot(..)) where

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

data TestBot = TestBot
  { aGameInfo :: A.ResponseGameInfo
  , aBotInfo  :: A.PlayerInfo
  , stepCount :: Int
  }
  | EmptyBot

instance Agent TestBot where
    race _ = C.Protoss
    step e@EmptyBot _ _ = return e
    step b obs abilities = do
        let msg = fromString $ "test hello " ++ (show $ (aGameInfo b) ^. #mapName) -- ++ show i -- ++ " " ++ show (playerInfo ^. #player_id) 
        let enemyBase = Utils.enemyBaseLocation (aGameInfo b) obs
        if stepCount b `mod` 10 == 0 then command [Actions.Chat msg, Actions.Chat (fromString.show $ enemyBase) ] else command []
        debug [Actions.DebugText "testU" (c ^. #pos) | c <- Utils.unitsSelf obs ]
        --tell [Actions.UnitCommand AbilityId.Attack (head (Utils.unitsSelf obs) ^. #tag) enemyBase ]
        --tell [Actions.Chat msg]
        return $ TestBot (aGameInfo b) (aBotInfo b) (stepCount b +1)