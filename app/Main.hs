module Main where

import SC2
import TestBot
import qualified Proto.S2clientprotocol.Common as A

main :: IO ()
main = startClient [Computer A.Protoss, Player EmptyBot]