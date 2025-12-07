module Main where

import Data.Text qualified as T
import SC2.Launcher.BotConfig
import SC2.Launcher.Game
import SC2.Launcher.LoadConfig
import SC2.Launcher.Participant
import SC2.Proto.Data (Map (LocalMap))
import System.Exit (exitFailure)
import TestBot

botParticipant :: Participant
botParticipant = Player EmptyBotAgent

main :: IO ()
main = do
  command <- parseCommandOptions
  case command of
    HostCommand opts -> runHost botParticipant opts
    JoinCommand opts -> runJoin botParticipant opts

runHost :: Participant -> HostCLI -> IO ()
runHost player cli = do
  cfg <- loadOrFail (hostConfigPath cli)
  case applyHostOverrides cfg cli of
    Left err -> failWith err
    Right adjusted -> playMatch (buildHostRuntime player adjusted)

runJoin :: Participant -> JoinCLI -> IO ()
runJoin player cli = do
  cfg <- loadOrFail (joinConfigPath cli)
  let adjusted = applyJoinOverrides cfg cli
  joinMatch (buildJoinRuntime player adjusted)

buildHostRuntime :: Participant -> BotConfig -> HostRuntime
buildHostRuntime player cfg =
  HostRuntime
    { hostParticipant = player
    , opponentParticipant = resolveOpponent player (opponent cfg)
    , hostNetwork = buildNetwork (general cfg)
    , hostStarcraft = starcraft2 cfg
    , hostMap = buildHostMap cfg
    }

buildJoinRuntime :: Participant -> BotConfig -> JoinRuntime
buildJoinRuntime player cfg =
  JoinRuntime
    { joinParticipant = player
    , joinNetwork = buildNetwork (general cfg)
    }

buildNetwork :: GeneralConfig -> NetworkSettings
buildNetwork GeneralConfig{hostName = h, portHost = hPort, portClient = cPort} =
  NetworkSettings
    { networkHostName = h
    , networkHostPort = hPort
    , networkClientPort = cPort
    }

resolveOpponent :: Participant -> OpponentConfig -> Participant
resolveOpponent _ (OpponentAI aiCfg) =
  Computer (aiRace aiCfg) (aiDifficulty aiCfg) (aiBuild aiCfg)
resolveOpponent fallback OpponentBot = fallback

buildHostMap :: BotConfig -> Map
buildHostMap cfg = LocalMap (T.pack (mapPath (host cfg))) Nothing

failWith :: String -> IO a
failWith err = do
  putStrLn $ "Configuration error: " <> err
  exitFailure

loadOrFail :: FilePath -> IO BotConfig
loadOrFail path = do
  result <- loadBotConfig path
  either failWith pure result
