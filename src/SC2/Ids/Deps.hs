{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module SC2.Ids.Deps where

import SC2.Ids.AbilityId (AbilityId(..))
import SC2.Ids.UnitTypeId (UnitTypeId(..))
import SC2.Ids.UpgradeId (UpgradeId)
import SC2.Ids.UpgradeResearchedFrom
import SC2.Proto.Data (UnitTypeData)
import UnitAbilities
import Utils

import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Aeson
import Data.Aeson.Lens
import Lens.Micro
import Data.Text (Text, unpack)
import Data.Maybe (mapMaybe)
import Data.List (nub)

-- Define types

-- Mapping from UnitTypeId to AbilityIds that train them
-- e.g. Marine <- [BARRACKSTRAIN_MARINE, ...]
type TrainDeps = HashMap.HashMap UnitTypeId [AbilityId]

generateTrainDeps :: Q [Dec]
generateTrainDeps = do
  content <- runIO $ B.readFile "data/data.json"
  let Just val = decode content :: Maybe Value

  -- Extract abilities
  let Just (Array abilities) = val ^? key "Ability"
      pairs = mapMaybe extractTrainDep (V.toList abilities)
      grouped = HashMap.fromListWith (++) pairs

  [d| trainDeps :: TrainDeps
      trainDeps = HashMap.fromList $(liftHashMap grouped)
    |]

-- Extract (UnitTypeId, [AbilityId]) for abilities with Train targets
extractTrainDep :: Value -> Maybe (UnitTypeId, [AbilityId])
extractTrainDep v = do
  aid <- v ^? key "id" . _Integral
  uid <- v ^? key "target" . key "Train" . key "produces" . _Integral
  utid <- lookupUnitTypeId (fromInteger uid)
  abid <- lookupAbilityId (fromInteger aid)
  return (utid, [abid])

-- Lift HashMap UnitTypeId [AbilityId] into Template Haskell expression
liftHashMap :: HashMap.HashMap UnitTypeId [AbilityId] -> Q Exp
liftHashMap = lift . HashMap.toList

-- Helper lookups using Enum instance and Bounded
unitTypeIdMap :: HashMap.HashMap Int UnitTypeId
unitTypeIdMap = HashMap.fromList [(fromEnum u, u) | u <- [minBound .. maxBound]]

abilityIdMap :: HashMap.HashMap Int AbilityId
abilityIdMap = HashMap.fromList [(fromEnum a, a) | a <- [minBound .. maxBound]]

lookupUnitTypeId :: Int -> Maybe UnitTypeId
lookupUnitTypeId = (`HashMap.lookup` unitTypeIdMap)

lookupAbilityId :: Int -> Maybe AbilityId
lookupAbilityId = (`HashMap.lookup` abilityIdMap)