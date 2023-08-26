
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module Bot(Bot(..), Dummy(..), TestBot(..)) where

import Actions qualified
import Data.String

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A

import Lens.Micro ( (&), (.~), (&), (.~), (^.) )

import Control.Monad
import Control.Monad.Writer.Strict

import qualified AbilityId

--import Control.Monad.Writer.Strict (Writer, tell, execWriter)

import Utils
import Proto.S2clientprotocol.Debug_Fields (unitTag)

class Bot s where
    step :: s -> A.Observation -> Writer [Actions.Action] s

newtype Dummy = Dummy Integer

instance Bot Dummy where
    step (Dummy i) obs = do
        let msg = fromString $ "dummy hello " ++ show i -- ++ " " ++ show (playerInfo ^. #player_id) 
        tell ([Actions.Chat msg | i `mod` 100 == 0])
        return $ Dummy (i+1)

data TestBot = TestBot
  { aGameInfo :: A.ResponseGameInfo
  , aBotInfo  :: A.PlayerInfo
  }

instance Bot TestBot where
    step b obs = do
        let msg = fromString $ "test hello " ++ (show $ (aGameInfo b) ^. #mapName) -- ++ show i -- ++ " " ++ show (playerInfo ^. #player_id) 
        let enemyBase = Utils.enemyBaseLocation (aGameInfo b) obs
        --tell [Actions.Chat msg, Actions.Chat (fromString.show $ enemyBase) ]
        tell [Actions.UnitCommand AbilityId.Attack ((head (Utils.unitsSelf obs)) ^. #tag) enemyBase ]
        --tell [Actions.Chat msg]
        return $ b