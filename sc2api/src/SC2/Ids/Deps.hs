{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SC2.Ids.Deps
  ( generateDeps
  , TrainDeps
  , BuildDeps
  , MorphDeps
  , Tech (..)
  , TechPath
  , TechPathCache
  ) where

import SC2.Ids.AbilityId (AbilityId (..))
import SC2.Ids.Ids
import SC2.Ids.UnitTypeId (UnitTypeId (..))
import SC2.Ids.UpgradeId (UpgradeId (..))

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State (..), execState, get, modify')
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Hashable
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Lens.Micro ((^..), (^?), (^?!))
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))

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

type TechPath = HashMap.HashMap Tech [Tech]

{- | Walk up parent directories from the splice-site source file
until a directory containing @data/data.json@ is found.
-}
findDataJson :: Q FilePath
findDataJson = do
  loc <- location
  let locFile = loc_filename loc
  runIO $ do
    absLoc <- makeAbsolute locFile
    go (takeDirectory absLoc)
 where
  go dir = do
    let candidate = dir </> "data" </> "data.json"
    exists <- doesFileExist candidate
    if exists
      then return candidate
      else
        let parent = takeDirectory dir
         in if parent == dir
              then error $ "data/data.json not found in any parent directory of: " ++ dir
              else go parent

type TechPathCache = HashMap Tech [Tech]

isTechUnit :: Tech -> Bool
isTechUnit (TechUnit _) = True
isTechUnit _ = False

isTechWorker (TechUnit u) = isUnitWorker u
isTechWorker _ = False

generateTechPathes dataFile = do
  content <- B.readFile dataFile
  let Just rootVal = decode content :: Maybe Value
      Just (Array abilitiesArray) = rootVal ^? key "Ability"
      abilitiesList = V.toList abilitiesArray
      Just (Array unitsArray) = rootVal ^? key "Unit"
      unitAbilitiesList = V.toList unitsArray

      unitAbilitiesGrouped :: HashMap UnitTypeId (HashMap AbilityId [Tech])
      unitAbilitiesGrouped =
        HashMap.map HashMap.fromList $
          HashMap.fromList $
            mapMaybe extractUnitAbilities unitAbilitiesList

      abilityProducerPairs = concatMap extractAbilityProducers unitAbilitiesList

      allProductionAbilities :: [AbilityId]
      allProductionAbilities = map snd trainPairs ++ map snd buildPairs ++ map snd morphPairs ++ map snd researchPairs

      abilityToProducerUnit :: HashMap.HashMap AbilityId UnitTypeId
      abilityToProducerUnit =
        HashMap.fromList
          [ (abid, minimum units)
          | abid <- Set.toList (Set.fromList allProductionAbilities)
          , let units =
                  [ uid
                  | (uid, abList) <- HashMap.toList unitAbilitiesGrouped
                  , any (\(a, _) -> a == abid) (HashMap.toList abList)
                  ]
          , not (null units)
          ]

      trainPairs = mapMaybe extractTrainDep abilitiesList
      buildPairs = mapMaybe extractBuildDep abilitiesList
      morphPairs = mapMaybe extractMorphDep abilitiesList
      researchPairs = mapMaybe extractResearchDep abilitiesList

      buildAbilitiesDict = HashMap.fromListWith (++) [(k, [v]) | (k, v) <- buildPairs]
      morphAbilitiesDict = HashMap.fromListWith (++) [(k, [v]) | (k, v) <- morphPairs]
      trainAbilitiesDict = HashMap.fromListWith (++) [(k, [v]) | (k, v) <- trainPairs]
      researchAbilitiesDict = HashMap.fromListWith (++) [(k, [v]) | (k, v) <- researchPairs]

      buildTechPath :: Tech -> State TechPathCache [Tech]
      buildTechPath start = do
        traceM $ "building techpath for " ++ show start
        path <- bfs [(start, [start])] HashSet.empty
        modify' (HashMap.insert start path)
        pure path
       where
        bfs [] _ =
          pure []
        bfs ((tech, path) : queue) visited = do
          let deps = directDeps tech

          traceM $
            "visiting "
              ++ show tech
              ++ " path: "
              ++ show path
              ++ " deps: "
              ++ show deps

          if null deps
            then do
              traceM $ "FOUND: " ++ show path
              pure path
            else
              let visited' = HashSet.insert tech visited
                  queue' =
                    queue
                      ++ [ (dep, dep : path)
                         | dep <- deps
                         , not (HashSet.member dep visited')
                         ]
               in bfs queue' visited'

      directDeps :: Tech -> [Tech]
      directDeps tech = fromMaybe [] (directDepsMaybe tech)

      directDepsMaybe :: Tech -> Maybe [Tech]
      directDepsMaybe (TechUpgrade uid) = do
        abilityIds <-
          HashMap.lookup uid researchAbilitiesDict
        traceM $ "research abilityIds: " ++ show abilityIds
        producer <- listToMaybe (mapMaybe (`HashMap.lookup` abilityToProducerUnit) abilityIds)

        traceM $ "producer: " ++ show producer

        producerAbilitiesWithDeps <- HashMap.lookup producer unitAbilitiesGrouped
        traceM $ "producerAbilitiesWithDeps: " ++ show producerAbilitiesWithDeps
        producingAbilityDep <-
          listToMaybe (mapMaybe (`HashMap.lookup` producerAbilitiesWithDeps) abilityIds)
        traceM $ "producingAbilityDep: " ++ show producingAbilityDep
        pure $
          if null producingAbilityDep
            then [TechUnit producer]
            else producingAbilityDep
      directDepsMaybe (TechUnit uid)
        | isUnitAddon uid =
            Nothing
        | isUnitStructure uid =
            let abilities =
                  HashMap.lookup uid buildAbilitiesDict
                    <|> HashMap.lookup uid morphAbilitiesDict

                resolve abilityId =
                  fromMaybe [] $ do
                    builder <- HashMap.lookup abilityId abilityToProducerUnit
                    traceM $ "Builder: " ++ show builder
                    deps <- HashMap.lookup builder unitAbilitiesGrouped
                    traceM $ "deps: " ++ show deps
                    HashMap.lookup abilityId deps
             in Just $
                  maybe [] (concatMap resolve) abilities
        | otherwise = do
            abilityIds <-
              HashMap.lookup uid trainAbilitiesDict
                <|> HashMap.lookup uid morphAbilitiesDict
            traceM $ "abilityIds: " ++ show abilityIds
            producer <- listToMaybe (mapMaybe (`HashMap.lookup` abilityToProducerUnit) abilityIds)

            traceM $ "producer: " ++ show producer

            producerAbilitiesWithDeps <- HashMap.lookup producer unitAbilitiesGrouped
            traceM $ "producerAbilitiesWithDeps: " ++ show producerAbilitiesWithDeps
            producingAbilityDep <-
              listToMaybe (mapMaybe (`HashMap.lookup` producerAbilitiesWithDeps) abilityIds)
            traceM $ "producingAbilityDep: " ++ show producingAbilityDep
            pure $
              if null producingAbilityDep
                then [TechUnit producer]
                else producingAbilityDep
      directDepsMaybe _ =
        Just [TechUnit ProtossStalker]

      allUnits = HashMap.keys unitAbilitiesGrouped
      allTechs = [TechUnit uid | uid <- allUnits] ++ [TechUpgrade uid | uid <- HashMap.keys researchAbilitiesDict]
      precomputeTechPaths :: [Tech] -> HashMap Tech [Tech]
      precomputeTechPaths techs = execState (mapM buildTechPath techs) HashMap.empty

      techPaths :: HashMap Tech [Tech]
      techPaths = precomputeTechPaths allTechs

  traceM $ "!!! " ++ show abilityProducerPairs
  traceM $ "====================================================================="
  traceM $ "!!! " ++ show unitAbilitiesGrouped
  return (techPaths, abilityProducerPairs, researchPairs, trainPairs)

generateDeps :: Q [Dec]
generateDeps = do
  dataFile <- findDataJson
  qAddDependentFile dataFile
  (techPaths, abilityProducerPairs, researchPairs, trainPairs) <- runIO (generateTechPathes dataFile)
  [d|
    trainDeps :: TrainDeps
    trainDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList trainPairs)

    --
    -- buildDeps :: BuildDeps
    -- buildDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList buildPairs)
    --
    -- morphDeps :: MorphDeps
    -- morphDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList morphPairs)
    --
    researchDeps :: ResearchDeps
    researchDeps = HashMap.fromList $(liftHashMap $ HashMap.fromList researchPairs)

    --
    -- unitAbilitiesDeps :: UnitAbilityDeps
    -- unitAbilitiesDeps = HashMap.fromList $(liftHashMap unitAbilitiesGrouped)
    --
    abilityExecutor :: AbilityProducer
    abilityExecutor = HashMap.fromList $(liftHashMap $ HashMap.fromList abilityProducerPairs)

    techPath :: TechPath
    techPath = HashMap.fromList $(liftHashMap techPaths)
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

extractAbilityProducers :: Value -> [(AbilityId, UnitTypeId)]
extractAbilityProducers unitValue =
  [ (toEnum . fromIntegral $ a, toEnum . fromIntegral $ unitValue ^?! key "id" . _Integral)
  | a <- unitValue ^.. key "abilities" . _Array . traverse . key "ability" . _Integral
  ]
