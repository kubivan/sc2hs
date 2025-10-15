{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SC2.BotConfig (
  BotConfig (..),
  GeneralConfig (..),
  HostConfig (..),
  StarCraft2Config (..),
  OpponentConfig (..),
  AIConfig (..),
) where

import Data.Aeson (FromJSON (..), withObject)
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types (Parser, camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Common qualified as C
import SC2.Proto.Data (Race)

data BotConfig = BotConfig
  { general :: GeneralConfig
  , host :: HostConfig
  , starcraft2 :: StarCraft2Config
  , opponent :: OpponentConfig
  }
  deriving (Show, Generic)

instance FromJSON BotConfig where
  parseJSON = genericParseJSON defaultOptions

data GeneralConfig = GeneralConfig
  { logLevel :: String
  , hostName :: String
  , portHost :: Int
  , portClient :: Int
  }
  deriving (Show, Generic)

instance FromJSON GeneralConfig where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_' }

data StarCraft2Config = StarCraft2Config
  { exePath :: FilePath
  , cwd :: FilePath
  , windowWidth :: Int
  , windowHeight :: Int
  , windowX :: Int
  , windowY :: Int
  , start :: Bool
  }
  deriving (Show, Generic)

instance FromJSON StarCraft2Config where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_' }

data HostConfig = HostConfig
  { mapPath :: FilePath
  }
  deriving (Show, Generic)

instance FromJSON HostConfig where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_' }

data OpponentConfig
  = OpponentAI AIConfig
  | OpponentBot
  deriving (Show)

instance FromJSON OpponentConfig where
  parseJSON = withObject "OpponentConfig" $ \o -> do
    typ <- o .: "type"
    case T.toLower typ of
      "ai" -> OpponentAI <$> o .: "ai"
      "bot" -> pure OpponentBot
      other -> fail $ "Unknown opponent type: " <> T.unpack other

data AIConfig = AIConfig
  { aiRace :: Race
  , aiDifficulty :: A.Difficulty
  , aiBuild :: Maybe A.AIBuild
  }
  deriving (Show)

instance FromJSON AIConfig where
  parseJSON = withObject "AIConfig" $ \o -> do
    raceTxt <- o .: "race"
    diffTxt <- o .: "difficulty"
    buildTxt <- o .:? "build"
    race <- parseRace raceTxt
    difficulty <- parseDifficulty diffTxt
    build <- traverse parseBuild buildTxt
    pure $ AIConfig race difficulty build

parseRace :: T.Text -> Parser Race
parseRace txt =
  case T.toLower txt of
    "protoss" -> pure C.Protoss
    "terran" -> pure C.Terran
    "zerg" -> pure C.Zerg
    "random" -> pure C.Random
    other -> fail $ "Unknown race: " <> T.unpack other

parseDifficulty :: T.Text -> Parser A.Difficulty
parseDifficulty txt =
  case T.toLower txt of
    "veryeasy" -> pure A.VeryEasy
    "easy" -> pure A.Easy
    "medium" -> pure A.Medium
    "mediumhard" -> pure A.MediumHard
    "hard" -> pure A.Hard
    "harder" -> pure A.Harder
    "veryhard" -> pure A.VeryHard
    "cheatvision" -> pure A.CheatVision
    "cheatmoney" -> pure A.CheatMoney
    "cheatinsane" -> pure A.CheatInsane
    other -> fail $ "Unknown difficulty: " <> T.unpack other

parseBuild :: T.Text -> Parser A.AIBuild
parseBuild txt =
  case T.toLower txt of
    "random" -> pure A.RandomBuild
    "rush" -> pure A.Rush
    "timing" -> pure A.Timing
    "power" -> pure A.Power
    "macro" -> pure A.Macro
    "air" -> pure A.Air
    other -> fail $ "Unknown AI build: " <> T.unpack other
