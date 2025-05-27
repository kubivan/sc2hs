module Main where

import SC2.Game
import SC2.Participant
import TestBot
import qualified Proto.S2clientprotocol.Common as A
import qualified Proto.S2clientprotocol.Sc2api as A

main :: IO ()
main = playMatch (Player EmptyBotAgent) (Computer A.Protoss A.Medium)
-- main = playMatch (Player EmptyBotAgent) (Player EmptyBotAgent)
