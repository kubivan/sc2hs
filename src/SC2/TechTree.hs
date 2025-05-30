
module SC2.TechTree where

import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (UnitTypeData)
import UnitAbilities
import Utils

import Lens.Micro
import Lens.Micro.Extras

import qualified Data.HashMap.Strict as HashMap
import Proto.S2clientprotocol.Data_Fields (techRequirement, maybe'techRequirement)
import Data.Hashable
import Data.Maybe
import Data.List
import Data.Set qualified as Set
import Units (fromEnum')
import Debug.Trace

type UnitTraits = HashMap.HashMap UnitTypeId UnitTypeData

data Tech = TechUnit UnitTypeId
  deriving (Show, Eq, Ord)

instance Hashable Tech where
  hashWithSalt s (TechUnit uid) = hashWithSalt s . fromEnum' $ uid

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
  return res `Utils.dbg` show (show uid, "needs ", res )
--techNeeds _ _ = Nothing


pathToTech :: UnitTraits -> Tech -> [Tech]
pathToTech traits tech = go tech [] where
    go t path = trace (show (t, path)) $ case techNeeds traits t of
        Nothing -> t : path `Utils.dbg` show (show t, "needs nothing")
        Just dep -> go dep (t:path)

buildTechDeps :: UnitTraits -> TechDeps
buildTechDeps traits = HashMap.fromListWith (++) [(tech, pathToTech traits tech) | tech <- TechUnit <$> HashMap.keys traits ]
