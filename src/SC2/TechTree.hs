{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module SC2.TechTree (
    UnitTraits,
    Tech (..),
    techPath,
    abilityToUnit,
    abilityToUnitSafe,
    unitToAbility,
) where

import SC2.Ids.AbilityId
import SC2.Ids.Deps
import SC2.Ids.UnitTypeId
import SC2.Ids.UpgradeId (UpgradeId)
import SC2.Proto.Data (UnitTypeData)
import UnitAbilities
import Utils

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Debug.Trace
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras
import Proto.S2clientprotocol.Data_Fields (maybe'techRequirement, techRequirement)
import Units (fromEnum')

$(generateDeps)

type UnitTraits = HashMap.HashMap UnitTypeId UnitTypeData

abilityToUnit :: UnitTraits -> AbilityId -> UnitTypeId
abilityToUnit traits a = case find (\x -> fromIntegral (x ^. #abilityId) == fromEnum a) (HashMap.elems traits) of
    Just t -> toEnum . fromIntegral $ t ^. #unitId
    Nothing -> error $ "abilityToUnit: invalid ability: " ++ show a

abilityToUnitSafe :: UnitTraits -> AbilityId -> Maybe UnitTypeId
abilityToUnitSafe traits a = case find (\x -> fromIntegral (x ^. #abilityId) == fromEnum a) (HashMap.elems traits) of
    Just t -> Just $ toEnum . fromIntegral $ t ^. #unitId
    Nothing -> Nothing

unitToAbility :: UnitTraits -> UnitTypeId -> AbilityId
unitToAbility traits uid = case traits HashMap.!? uid of
    Just t -> toEnum . fromIntegral $ t ^. #abilityId
    Nothing -> error $ "unitToAbility: invalid id: " ++ show uid

saveDeps :: FilePath -> TechPath -> IO ()
saveDeps path deps = B.writeFile path (Pretty.encodePretty deps)

loadDeps :: FilePath -> IO (Maybe TechPath)
loadDeps path = Aeson.decode <$> B.readFile path
