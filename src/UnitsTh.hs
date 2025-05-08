{-# LANGUAGE TemplateHaskell #-}

module UnitsTh (genBuildingMapping) where

import Language.Haskell.TH
import Data.List
import Data.Maybe
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
--import Data.Char (isPrefixOf)

-- Generates a list of (UnitTypeId, AbilityId) pairs based on naming conventions
genBuildingMapping :: Name -> Name -> Q [Dec]
genBuildingMapping unitTypeName abilityIdName = do
  -- Get all the constructors for UnitTypeId and AbilityId
  unitTypeInfo <- reify unitTypeName
  abilityIdInfo <- reify abilityIdName

  let unitTypeConstructors = getConstructors unitTypeInfo
      abilityIdConstructors = getConstructors abilityIdInfo

  -- Generate the mapping using naming conventions (e.g., ProtossNexus -> BuildNexus)
  let mapping = [ (unitType, fromJust buildCmd)
                | unitType <- unitTypeConstructors
                , let buildCmd = mkBuildCmd (nameBase unitType)
                , isJust buildCmd
                , (fromJust buildCmd) `elem` map nameBase abilityIdConstructors
                ]

  -- Create the mapping list at compile-time
  let mappingList = listE [tupE [conE unitType, conE (mkName buildCmd)] | (unitType, buildCmd) <- mapping]

  -- Define the `buildingMapping` as a top-level declaration
  sequence [valD (varP (mkName "buildingMapping")) (normalB mappingList) []]

-- Helper to extract constructors from a data type
getConstructors :: Info -> [Name]
getConstructors (TyConI (DataD _ _ _ _ constructors _)) = [conName | NormalC conName _ <- constructors]
getConstructors _ = []

-- Helper to create Build* AbilityId from Protoss* UnitTypeId
mkBuildCmd :: String -> Maybe String
mkBuildCmd unitTypeStr =
  case stripPrefix "Protoss" unitTypeStr of
    Just stripped -> Just $ "Build" ++ stripped
    Nothing -> Nothing --error $ "Unexpected unit type: " ++ unitTypeStr