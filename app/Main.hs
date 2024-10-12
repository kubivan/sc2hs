module Main where

import SC2
import TestBot
import qualified Proto.S2clientprotocol.Common as A

main :: IO ()
main = runGame [Computer A.Protoss, Player Opening]
--main = runGame [Player Opening, Player Opening]