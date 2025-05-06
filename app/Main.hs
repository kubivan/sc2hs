module Main where

import SC2.Game
import TestBot
import qualified Proto.S2clientprotocol.Common as A

main :: IO ()
main = runGame (Player Opening) (Computer A.Protoss)
-- main = runGame (Player Opening) (Player Opening)