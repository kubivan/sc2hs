
{-# LANGUAGE GADTs #-}

module SC2.Participant where

import Agent
import SC2.Proto.Data(Race)
import qualified Proto.S2clientprotocol.Sc2api as A

data Participant where
    Computer :: Race -> A.Difficulty -> Maybe A.AIBuild -> Participant
    Player :: (Agent a) => a -> Participant