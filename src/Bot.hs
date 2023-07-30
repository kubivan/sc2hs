
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Bot(Bot(..), Dummy(..)) where

import Actions qualified

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A

import Control.Monad
import Control.Monad.Writer.Strict

--import Control.Monad.Writer.Strict (Writer, tell, execWriter)

class Bot s where
    step :: s -> A.Observation -> Writer [Actions.Action] s

data Dummy = Dummy Integer

instance Bot Dummy where 
    step (Dummy i) obs = do
        let msg = "dummy hello"
        tell [Actions.Chat msg]
        return $ Dummy (i+1)