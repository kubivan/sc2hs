
{-# LANGUAGE GADTs #-}

module SC2.Participant where

import Agent
import SC2.Proto.Data(Race)

data Participant where
    Computer :: Race -> Participant
    Player :: (Agent a) => a -> Participant