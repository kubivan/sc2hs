{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveAnyClass #-}

module SC2.Ids.AbilityId (AbilityId(..), toEnum, fromEnum, isBuildAbility) where

import SC2.Ids.Ids
import Language.Haskell.TH.Syntax (Lift)

import Data.List (isPrefixOf, isInfixOf)
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Text.Read (readMaybe)
import Data.Text (unpack)

import Data.Hashable

instance Hashable AbilityId where
  hash = fromEnum
  hashWithSalt s val = fromEnum val + s

--TODO: move to TH part
isBuildAbility:: AbilityId -> Bool
isBuildAbility x = "BUILD" `isInfixOf` show x

instance ToJSON AbilityId where
  toJSON = toJSON . show

instance FromJSON AbilityId where
  parseJSON = withText "AbilityId" $ \txt ->
    case readMaybe (unpack txt) of
      Just utid -> pure utid
      Nothing -> fail $ "Invalid AbilityId: " ++ unpack txt
