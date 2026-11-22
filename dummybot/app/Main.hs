module Main (main) where

import Data.Text qualified as Text
import DummyBot (DummyBot (..))
import Proto.S2clientprotocol.Sc2api qualified as A
import SC2.Launcher.BotConfig
import SC2.Launcher.Game
import SC2.Launcher.LoadConfig
import SC2.Launcher.Participant
import SC2.Proto.Data (Map (LocalMap))
import System.Exit (exitFailure)

main :: IO ()
main = do
  command <- parseCommandOptions
  case command of
    HostCommand cli -> do
      cfg <- loadAndConfigure (hostConfigPath cli) (\loaded -> applyHostOverrides loaded cli)
      playMatch (hostRuntime cfg)
    JoinCommand cli -> do
      cfg <- loadAndConfigure (joinConfigPath cli) (\loaded -> Right (applyJoinOverrides loaded cli))
      joinMatch (joinRuntime cfg)

loadAndConfigure :: FilePath -> (BotConfig -> Either String BotConfig) -> IO BotConfig
loadAndConfigure path configure = do
  loaded <- loadBotConfig path
  case loaded >>= configure of
    Left err -> putStrLn err >> exitFailure
    Right cfg -> pure cfg

hostRuntime :: BotConfig -> HostRuntime
hostRuntime cfg =
  HostRuntime
    { hostParticipant = Player DummyBot
    , opponentParticipant = resolveOpponentParticipant (opponent cfg)
    , hostNetwork = networkSettings (general cfg)
    , hostStarcraft = starcraft2 cfg
    , hostMap = LocalMap (Text.pack (mapPath (host cfg))) Nothing
    }

resolveOpponentParticipant :: OpponentConfig -> Participant
resolveOpponentParticipant (OpponentAI ai) = Computer (aiRace ai) (aiDifficulty ai) (aiBuild ai)
resolveOpponentParticipant OpponentBot = Player DummyBot

joinRuntime :: BotConfig -> JoinRuntime
joinRuntime cfg =
  JoinRuntime
    { joinParticipant = Player DummyBot
    , joinNetwork = networkSettings (general cfg)
    }

networkSettings :: GeneralConfig -> NetworkSettings
networkSettings cfg =
  NetworkSettings
    { networkHostName = hostName cfg
    , networkHostPort = portHost cfg
    , networkClientPort = portClient cfg
    }
