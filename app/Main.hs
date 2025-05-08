module Main where

import SC2.Game
import SC2.Participant
import TestBot
import qualified Proto.S2clientprotocol.Common as A

main :: IO ()
main = playMatch (Player EmptyBotAgent) (Computer A.Protoss)
--main = playMatch (Player Opening) (Player Opening)
