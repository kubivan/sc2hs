{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO: export list
module SC2.TechTree where

import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Ids.UpgradeId (UpgradeId)
import SC2.Ids.UpgradeResearchedFrom
import SC2.Proto.Data (UnitTypeData)
import UnitAbilities
import Utils

import Lens.Micro
import Lens.Micro.Extras

import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace
import Proto.S2clientprotocol.Data_Fields (maybe'techRequirement, techRequirement)
import Units (fromEnum')

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)

type UnitTraits = HashMap.HashMap UnitTypeId UnitTypeData

data Tech = TechUnit UnitTypeId | TechUpgrade UpgradeId
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance Hashable Tech where
    hashWithSalt s (TechUnit uid) = hashWithSalt s . fromEnum' $ uid
    hashWithSalt s (TechUpgrade uid) = hashWithSalt s . fromEnum' $ uid

type TechDeps = HashMap.HashMap Tech [Tech]

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

techNeeds :: UnitTraits -> Tech -> Maybe Tech
techNeeds traits (TechUnit uid) = do
    uData <- HashMap.lookup uid traits
    requirement <- uData ^. #maybe'techRequirement
    res <- return . TechUnit $ toEnum $ fromIntegral requirement
    return res `Utils.dbg` show (show uid, "needs ", res)
techNeeds _ (TechUpgrade uid) = TechUnit <$> upgradeDeps HashMap.!? uid

pathToTech :: UnitTraits -> Tech -> [Tech]
pathToTech traits tech = go tech []
  where
    go t path = trace (show (t, path)) $ case techNeeds traits t of
        Nothing -> t : path `Utils.dbg` show (show t, "needs nothing")
        Just dep -> go dep (t : path)

buildTechDeps :: UnitTraits -> TechDeps
buildTechDeps traits =
    HashMap.fromListWith
        (++)
        [ (tech, pathToTech traits tech)
        | tech <- (TechUnit <$> HashMap.keys traits) ++ (TechUpgrade <$> HashMap.keys upgradeDeps)
        ]

saveDeps :: FilePath -> TechDeps -> IO ()
saveDeps path deps = B.writeFile path (Pretty.encodePretty deps)

loadDeps :: FilePath -> IO (Maybe TechDeps)
loadDeps path = Aeson.decode <$> B.readFile path
