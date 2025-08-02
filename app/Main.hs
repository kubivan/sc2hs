module Main where

import Proto.S2clientprotocol.Common qualified as A
import Proto.S2clientprotocol.Sc2api qualified as A
import SC2.Game
import SC2.Participant
import System.Environment (getArgs)
import TestBot

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--host"] -> playMatch (Player EmptyBotAgent) (Computer A.Protoss A.Medium)
    ["--join"] -> joinMatch EmptyBotAgent
    _ -> putStrLn usage

usage :: String
usage =
  unlines
    [ "Usage:",
      "  --host Start a match vs computer",
      "  --join Join a hosted game"
    ]
