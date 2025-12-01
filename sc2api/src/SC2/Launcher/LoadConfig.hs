{-# LANGUAGE OverloadedStrings #-}

module SC2.Launcher.LoadConfig
  ( CommandOptions (..),
    HostCLI (..),
    JoinCLI (..),
    NetworkOverrides (..),
    OpponentOverrides (..),
    OpponentTypeOverride (..),
    parseCommandOptions,
    loadBotConfig,
    applyHostOverrides,
    applyJoinOverrides,
  )
where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust)
import Data.Yaml (decodeFileEither)
import Options.Applicative
import Paths_sc2api (getDataFileName)
import Proto.S2clientprotocol.Common qualified as C
import Proto.S2clientprotocol.Sc2api qualified as A
import SC2.Launcher.BotConfig
import System.Directory (doesFileExist)

data CommandOptions
  = HostCommand HostCLI
  | JoinCommand JoinCLI
  deriving (Show)

data HostCLI = HostCLI
  { hostConfigPath :: FilePath,
    hostNetworkOverrides :: NetworkOverrides,
    hostStartOverride :: Maybe Bool,
    hostOpponentOverrides :: OpponentOverrides
  }
  deriving (Show)

data JoinCLI = JoinCLI
  { joinConfigPath :: FilePath,
    joinNetworkOverrides :: NetworkOverrides
  }
  deriving (Show)

data NetworkOverrides = NetworkOverrides
  { overrideHostName :: Maybe String
  , overridePortHost :: Maybe Int
  , overridePortClient :: Maybe Int
  }
  deriving (Show)

data OpponentTypeOverride
  = OpponentTypeAI
  | OpponentTypeBot
  deriving (Eq, Show)

data OpponentOverrides = OpponentOverrides
  { overrideOpponentType :: Maybe OpponentTypeOverride
  , overrideAIRace :: Maybe C.Race
  , overrideAIDifficulty :: Maybe A.Difficulty
  , overrideAIBuild :: Maybe (Maybe A.AIBuild)
  }
  deriving (Show)

parseCommandOptions :: IO CommandOptions
parseCommandOptions = customExecParser prefsParser (info (commandParser <**> helper) fullDesc)
  where
    prefsParser = prefs showHelpOnError

commandParser :: Parser CommandOptions
commandParser = hsubparser (host <> join)
  where
    host = command "host" (info (HostCommand <$> hostParser) (progDesc "Host a game"))
    join = command "join" (info (JoinCommand <$> joinParser) (progDesc "Join an existing game"))

hostParser :: Parser HostCLI
hostParser =
  HostCLI
    <$> configOption
    <*> networkParser True
    <*> startOverrideParser
    <*> opponentParser

joinParser :: Parser JoinCLI
joinParser =
  JoinCLI
    <$> configOption
    <*> networkParser False

configOption :: Parser FilePath
configOption =
  strOption
    ( long "config"
        <> metavar "PATH"
        <> value "bot-config.yaml"
        <> showDefault
        <> help "Path to the configuration file"
    )

networkParser :: Bool -> Parser NetworkOverrides
networkParser includeClientPort =
  NetworkOverrides
    <$> optional (strOption (long "host-name" <> metavar "HOST" <> help "Override host name"))
    <*> optional (option auto (long "port-host" <> metavar "PORT" <> help "Override host port"))
    <*> clientOverride
  where
    clientOverride
      | includeClientPort = optional (option auto (long "port-client" <> metavar "PORT" <> help "Override client port"))
      | otherwise = pure Nothing

startOverrideParser :: Parser (Maybe Bool)
startOverrideParser =
  optional
    ( flag'
        True
        ( long "start-sc2"
            <> help "Start StarCraft II before connecting"
        )
        <|> flag'
          False
          ( long "no-start-sc2"
              <> help "Do not attempt to start StarCraft II"
          )
    )

opponentParser :: Parser OpponentOverrides
opponentParser =
  OpponentOverrides
    <$> optional (option (eitherReader parseOpponentType) (long "opponent-type" <> metavar "ai|bot" <> help "Override opponent type"))
    <*> optional (option (eitherReader parseRace) (long "ai-race" <> metavar "RACE" <> help "Override AI race"))
    <*> optional (option (eitherReader parseDifficulty) (long "ai-difficulty" <> metavar "DIFFICULTY" <> help "Override AI difficulty"))
    <*> optional (option (eitherReader parseBuildOverride) (long "ai-build" <> metavar "BUILD|none" <> help "Override AI build or pass 'none' to remove"))

parseOpponentType :: String -> Either String OpponentTypeOverride
parseOpponentType str =
  case lowercase str of
    "ai" -> Right OpponentTypeAI
    "bot" -> Right OpponentTypeBot
    _ -> Left "Expected opponent type 'ai' or 'bot'"

parseRace :: String -> Either String C.Race
parseRace str =
  case lowercase str of
    "protoss" -> Right C.Protoss
    "terran" -> Right C.Terran
    "zerg" -> Right C.Zerg
    "random" -> Right C.Random
    _ -> Left "Unsupported race. Use protoss, terran, zerg, or random"

parseDifficulty :: String -> Either String A.Difficulty
parseDifficulty str =
  case lowercase str of
    "veryeasy" -> Right A.VeryEasy
    "easy" -> Right A.Easy
    "medium" -> Right A.Medium
    "mediumhard" -> Right A.MediumHard
    "hard" -> Right A.Hard
    "harder" -> Right A.Harder
    "veryhard" -> Right A.VeryHard
    "cheatvision" -> Right A.CheatVision
    "cheatmoney" -> Right A.CheatMoney
    "cheatinsane" -> Right A.CheatInsane
    _ -> Left "Unsupported difficulty"

parseBuildOverride :: String -> Either String (Maybe A.AIBuild)
parseBuildOverride str =
  case lowercase str of
    "none" -> Right Nothing
    "random" -> Right (Just A.RandomBuild)
    "rush" -> Right (Just A.Rush)
    "timing" -> Right (Just A.Timing)
    "power" -> Right (Just A.Power)
    "macro" -> Right (Just A.Macro)
    "air" -> Right (Just A.Air)
    _ -> Left "Unsupported AI build"

lowercase :: String -> String
lowercase = map toLower

loadBotConfig :: FilePath -> IO (Either String BotConfig)
loadBotConfig path = do
  -- Try to find the config file in this order:
  -- 1. Provided path (if it exists)
  -- 2. Installed data directory (from stack install)
  configPath <- findConfigFile path
  result <- decodeFileEither configPath
  pure $ either (Left . show) Right result

findConfigFile :: FilePath -> IO FilePath
findConfigFile path = do
  existsAtPath <- doesFileExist path
  if existsAtPath
    then pure path
    else getDataFileName path -- Falls back to installed data file

applyHostOverrides :: BotConfig -> HostCLI -> Either String BotConfig
applyHostOverrides cfg cli = do
  opponent' <- applyOpponent (hostOpponentOverrides cli) (opponent cfg)
  let general' = applyNetwork (hostNetworkOverrides cli) (general cfg)
      starcraft' = applyStart (hostStartOverride cli) (starcraft2 cfg)
  pure cfg {general = general', starcraft2 = starcraft', opponent = opponent'}

applyJoinOverrides :: BotConfig -> JoinCLI -> BotConfig
applyJoinOverrides cfg cli =
  cfg
    { general = applyNetwork (joinNetworkOverrides cli) (general cfg)
    }

applyNetwork :: NetworkOverrides -> GeneralConfig -> GeneralConfig
applyNetwork overrides gen =
  gen
    { hostName = fromMaybe (hostName gen) (overrideHostName overrides),
      portHost = fromMaybe (portHost gen) (overridePortHost overrides),
      portClient = fromMaybe (portClient gen) (overridePortClient overrides)
    }

applyStart :: Maybe Bool -> StarCraft2Config -> StarCraft2Config
applyStart Nothing sc2 = sc2
applyStart (Just flag) sc2 = sc2 {start = flag}

applyOpponent :: OpponentOverrides -> OpponentConfig -> Either String OpponentConfig
applyOpponent overrides base =
  case overrideOpponentType overrides of
    Just OpponentTypeBot ->
      if hasAIOverrides overrides
        then Left "AI overrides require opponent-type=ai"
        else Right OpponentBot
    Just OpponentTypeAI -> OpponentAI <$> buildAIConfig overrides (extractAI base)
    Nothing ->
      case base of
        OpponentAI cfg -> OpponentAI <$> updateAI cfg overrides
        OpponentBot ->
          if hasAIOverrides overrides
            then Left "Set opponent-type=ai to adjust AI parameters"
            else Right OpponentBot

extractAI :: OpponentConfig -> Maybe AIConfig
extractAI (OpponentAI cfg) = Just cfg
extractAI OpponentBot = Nothing

buildAIConfig :: OpponentOverrides -> Maybe AIConfig -> Either String AIConfig
buildAIConfig overrides base =
  let race = overrideAIRace overrides <|> fmap aiRace base
      difficulty = overrideAIDifficulty overrides <|> fmap aiDifficulty base
      buildOverride = overrideAIBuild overrides
      build = case buildOverride of
        Nothing -> base >>= aiBuild
        Just value -> value
   in case (race, difficulty) of
        (Just r, Just d) -> pure $ AIConfig r d build
        _ -> Left "AI opponent requires both race and difficulty"

updateAI :: AIConfig -> OpponentOverrides -> Either String AIConfig
updateAI cfg overrides =
  let race = fromMaybe (aiRace cfg) (overrideAIRace overrides)
      difficulty = fromMaybe (aiDifficulty cfg) (overrideAIDifficulty overrides)
      build = case overrideAIBuild overrides of
        Nothing -> aiBuild cfg
        Just value -> value
   in pure $ AIConfig race difficulty build

hasAIOverrides :: OpponentOverrides -> Bool
hasAIOverrides overrides =
  isJust (overrideAIRace overrides)
    || isJust (overrideAIDifficulty overrides)
    || isJust (overrideAIBuild overrides)
