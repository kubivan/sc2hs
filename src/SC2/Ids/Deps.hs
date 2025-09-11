{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SC2.Ids.Deps (
    generateDeps,
    TrainDeps,
    BuildDeps,
    MorphDeps,
    Tech (..),
    TechPath,
) where

import SC2.Ids.AbilityId (AbilityId (..))
import SC2.Ids.UnitTypeId (UnitTypeId (..))
import SC2.Ids.UpgradeId (UpgradeId (..))

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Lens.Micro ((^..), (^?), (^?!))

import Utils (dbg)

-- Define types

data Tech = TechUnit UnitTypeId | TechUpgrade UpgradeId | TechAbility AbilityId
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Lift)

instance Hashable Tech where
    hashWithSalt s (TechUnit uid) = hashWithSalt s . fromEnum $ uid
    hashWithSalt s (TechUpgrade uid) = hashWithSalt s . fromEnum $ uid
    hashWithSalt s (TechAbility aid) = hashWithSalt s . fromEnum $ aid

type TrainDeps = HashMap.HashMap UnitTypeId AbilityId
type BuildDeps = HashMap.HashMap UnitTypeId AbilityId
type MorphDeps = HashMap.HashMap UnitTypeId AbilityId
type ResearchDeps = HashMap.HashMap UpgradeId AbilityId

type AbilityProducer = HashMap.HashMap AbilityId UnitTypeId

type UnitAbilityDeps = HashMap.HashMap UnitTypeId [(AbilityId, [Tech])]

type TechDeps = HashMap.HashMap Tech [Tech]
type TechPath = HashMap.HashMap Tech [Tech]

generateDeps :: Q [Dec]
generateDeps = do
    content <- runIO $ B.readFile "data/data.json"
    let Just rootVal = decode content :: Maybe Value
        Just (Array abilitiesArray) = rootVal ^? key "Ability"
        abilitiesList = V.toList abilitiesArray

        Just (Array unitsArray) = rootVal ^? key "Unit"
        unitAbilitiesList = V.toList unitsArray

        trainPairs = mapMaybe extractTrainDep abilitiesList
        buildPairs = mapMaybe extractBuildDep abilitiesList
        morphPairs = mapMaybe extractMorphDep abilitiesList
        researchPairs = mapMaybe extractResearchDep abilitiesList

        unitAbilitiesGrouped :: UnitAbilityDeps
        unitAbilitiesGrouped = HashMap.fromList $ mapMaybe extractUnitAbilities unitAbilitiesList

        --abilityProducer :: AbilityProducer
        abilityProducerPairs = concatMap extractAbilityProducers unitAbilitiesList

        techAbilityDeps :: [(Tech, [Tech])]
        techAbilityDeps =
            [ (TechAbility abid, deps)
            | abList <- HashMap.elems unitAbilitiesGrouped
            , (abid, deps) <- abList
            ]

        techUnitBuilds :: [(Tech, [Tech])]
        techUnitBuilds =
            [ (TechUnit uid, [TechAbility abid])
            | (uid, abid) <- trainPairs
            ]

        techBuildDeps :: [(Tech, [Tech])]
        techBuildDeps =
            [ (TechUnit uid, [TechAbility abid])
            | (uid, abid) <- buildPairs
            ]

        techUpgradeDeps :: [(Tech, [Tech])]
        techUpgradeDeps =
            [ (TechUpgrade uid, [TechAbility abid])
            | (uid, abid) <- researchPairs
            ]

        extraAbilityLinks :: [(Tech, [Tech])]
        extraAbilityLinks =
            [ (TechAbility abid, [TechUnit uid])
            | (uid, abList) <- HashMap.toList unitAbilitiesGrouped
            , (abid, _) <- abList
            ]

        -- Final direct dependency graph
        techDeps :: TechDeps
        techDeps =
            HashMap.fromListWith
                (++)
                (techAbilityDeps ++ techUnitBuilds ++ techUpgradeDeps ++ techBuildDeps ++ extraAbilityLinks)

        -- Build full paths (transitive closure)
        rawTechPath :: TechPath
        rawTechPath = buildTechPaths techDeps

        -- Remove TechAbility from final path values
        isTechAbility (TechAbility _) = True
        isTechAbility _ = False

        techPathFiltered :: TechPath
        techPathFiltered = HashMap.map (filter (not . isTechAbility)) rawTechPath

    [d|
        trainDeps :: TrainDeps
        trainDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList trainPairs)

        buildDeps :: BuildDeps
        buildDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList buildPairs)

        morphDeps :: MorphDeps
        morphDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList morphPairs)

        researchDeps :: ResearchDeps
        researchDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList researchPairs)

        unitAbilitiesDeps :: UnitAbilityDeps
        unitAbilitiesDeps = HashMap.fromList $(liftHashMap unitAbilitiesGrouped)

        abilityExecutor :: AbilityProducer
        abilityExecutor = HashMap.fromList $(liftHashMap $ HashMap.fromList abilityProducerPairs)

        techDeps :: TechDeps
        techDeps =
            HashMap.fromListWith
                (++)
                $( liftHashMap $
                    HashMap.fromListWith
                        (++)
                        (techAbilityDeps ++ techUnitBuilds ++ techUpgradeDeps ++ techBuildDeps ++ extraAbilityLinks)
                 )

        techPath :: TechPath
        techPath = HashMap.fromList $(liftHashMap techPathFiltered)
        |]

extractTrainDep :: Value -> Maybe (UnitTypeId, AbilityId)
extractTrainDep v = do
    aid <- v ^? key "id" . _Integral
    uid <- v ^? key "target" . key "Train" . key "produces" . _Integral
    let utid = toEnum (fromInteger uid)
        abid = toEnum (fromInteger aid)
    return (utid, abid)

extractBuildDep :: Value -> Maybe (UnitTypeId, AbilityId)
extractBuildDep v = do
    aid <- v ^? key "id" . _Integral
    tgt <- v ^? key "target"
    uid <-
        (tgt ^? key "Build" . key "produces" . _Integral)
            <|> (tgt ^? key "BuildOnUnit" . key "produces" . _Integral)
            <|> (tgt ^? key "BuildInstant" . key "produces" . _Integral)
    let utid = toEnum (fromInteger uid)
        abid = toEnum (fromInteger aid)
    return (utid, abid)

extractMorphDep :: Value -> Maybe (UnitTypeId, AbilityId)
extractMorphDep v = do
    aid <- v ^? key "id" . _Integral
    uid <-
        (v ^? key "target" . key "Morph" . key "produces" . _Integral)
            <|> (v ^? key "target" . key "MorphPlace" . key "produces" . _Integral)
    let utid = toEnum (fromInteger uid)
        abid = toEnum (fromInteger aid)
    return (utid, abid)

extractResearchDep :: Value -> Maybe (UpgradeId, AbilityId)
extractResearchDep v = do
    aid <- v ^? key "id" . _Integral
    uid <- v ^? key "target" . key "Research" . key "upgrade" . _Integral
    let upid = toEnum (fromInteger uid)
        abid = toEnum (fromInteger aid)
    return (upid, abid)

extractUnitAbilities :: Value -> Maybe (UnitTypeId, [(AbilityId, [Tech])])
extractUnitAbilities v = do
    uid <- v ^? key "id" . _Integral
    let abdeps = extractAbilities v
        utid = toEnum (fromInteger uid)
    return (utid, abdeps)

extractAbilities :: Value -> [(AbilityId, [Tech])]
extractAbilities v = extractAbilityDeps <$> v ^.. key "abilities" . values

extractAbilityDeps :: Value -> (AbilityId, [Tech])
extractAbilityDeps obj =
    let abid = obj ^?! key "ability" . _Integral
        upgradeDeps = obj ^.. key "requirements" . _Array . traverse . key "upgrade" . _Integral
        unitDeps = obj ^.. key "requirements" . _Array . traverse . key "building" . _Integral
     in (toEnum abid, (TechUpgrade . toEnum <$> upgradeDeps) ++ (TechUnit . toEnum <$> unitDeps))

liftHashMap :: (Lift k, Lift v) => HashMap.HashMap k v -> Q Exp
liftHashMap = lift . HashMap.toList

buildTechPaths :: TechDeps -> TechPath
buildTechPaths deps = HashMap.fromList [(t, ordNub . reverse $ go t Set.empty []) | t <- HashMap.keys deps]
  where
    go :: Tech -> Set.Set Tech -> [Tech] -> [Tech]
    go tech visited acc
        | tech `Set.member` visited = acc
        | otherwise =
            let visited' = Set.insert tech visited
                children = fromMaybe [] (HashMap.lookup tech deps)
                acc' = foldl' (\a child -> go child visited' a) acc children
             in tech : acc'

reverseHashMap :: (Hashable v) => HashMap k [v] -> HashMap v [k]
reverseHashMap input =
    HashMap.fromListWith (++) $
        [(v, [k]) | (k, vs) <- HashMap.toList input, v <- vs]

ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert x seen) xs

extractAbilityProducers :: Value -> [(AbilityId, UnitTypeId)]
extractAbilityProducers unitValue = [(toEnum . fromIntegral $ a, toEnum . fromIntegral $ unitValue ^?! key "id" . _Integral )
    | a <- unitValue ^.. key "abilities" . _Array . traverse . key "ability" . _Integral]
