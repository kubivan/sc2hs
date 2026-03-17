{-# LANGUAGE GADTs #-}

module SC2.Launcher.Participant where

import Agent
import Proto.S2clientprotocol.Sc2api qualified as A
import SC2.Proto.Data (Race)

data Participant where
    Computer :: Race -> A.Difficulty -> Maybe A.AIBuild -> Participant
    Player :: (Agent a) => a -> Participant
