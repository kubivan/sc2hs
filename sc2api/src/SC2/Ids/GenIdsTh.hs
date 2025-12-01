{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SC2.Ids.GenIdsTh (generateTechTypeStuff) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy qualified as B
import Data.Char (isAlphaNum, toUpper)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.String
import Data.Text (Text, unpack)
import Data.Vector qualified as V
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Lens.Micro

-- Template Haskell gene rator for UnitTypeId
type UnitId = Int
type UnitName = Name -- the constructor, e.g. ProtossZealot
type Race = Text

jsonDataFileName = "data/data.json"

class TechEnum t where
    techName :: t -> Name
    techId :: t -> Int

    -- TODO: there shoulde be better way: t is not needed, use typefamilies or smth
    techTypeName :: t -> String
    techInvalidTypeName :: t -> String

data ParsedAbility = ParsedAbility
    { abilityId :: Int
    , abilityName :: UnitName
    }

data ParsedUnit = ParsedUnit
    { unitId :: UnitId
    , unitName :: UnitName
    , unitRace :: Race
    , traits :: HashMap String Bool
    }

instance TechEnum ParsedAbility where
    techName = abilityName
    techId = abilityId
    techTypeName _ = "AbilityId"
    techInvalidTypeName _ = "InvalidAbilityId"

instance TechEnum ParsedUnit where
    techName = unitName
    techId = unitId
    techTypeName _ = "UnitTypeId"
    techInvalidTypeName _ = "InvalidUnitTypeId"

parseAbilities :: Value -> Q [ParsedAbility]
parseAbilities val = do
    let Just (Array abs) = val ^? key "Ability"
    return $ catMaybes $ map parseAbiblity (V.toList abs)

parseAbiblity :: Value -> Maybe ParsedAbility
parseAbiblity v = do
    uid <- v ^? key "id" . _Integer
    name <- v ^? key "name" . _String
    let constructor = mkName (sanitize name)
    return $ ParsedAbility (fromInteger uid) constructor

parseUnits :: Value -> Q [ParsedUnit]
parseUnits val = do
    let Just (Array units) = val ^? key "Unit"
    return $ catMaybes $ map parseUnit (V.toList units)

parseUnit :: Value -> Maybe ParsedUnit
parseUnit v = do
    uid <- v ^? key "id" . _Integer
    name <- v ^? key "name" . _String
    race <- v ^? key "race" . _String
    let constructor = mkName (sanitize race ++ sanitize name)
        keys = ["is_structure", "is_worker", "is_townhall", "is_addon", "needs_power"]
        boolTraits =
            HashMap.fromList
                [(k, b) | k <- keys, Just b <- [v ^? key (fromString k) . _Bool]]
    return $ ParsedUnit (fromInteger uid) constructor race boolTraits

genDataDecl :: (TechEnum t) => [t] -> Dec
genDataDecl values =
    DataD
        []
        (mkName (techTypeName (head values)))
        []
        Nothing
        (NormalC invalidCon [] : map (\u -> NormalC (techName u) []) values)
        [DerivClause Nothing (map ConT [''Eq, ''Ord, ''Show, ''Lift, ''Read])]
  where
    invalidCon = mkName (techInvalidTypeName (head values))

genEnumInstance :: (TechEnum t) => [t] -> Dec
genEnumInstance values =
    InstanceD
        Nothing
        []
        (AppT (ConT ''Enum) (ConT (mkName (techTypeName (head values)))))
        [ FunD
            'fromEnum
            ( Clause [ConP (mkName (techInvalidTypeName (head values))) [] []] (NormalB (LitE (IntegerL 0))) []
                : [ Clause
                        [ConP (techName u) [] []]
                        (NormalB (LitE (IntegerL (fromIntegral (techId u)))))
                        []
                  | u <- values
                  ]
            )
        , FunD 'toEnum $
            Clause [LitP (IntegerL 0)] (NormalB (ConE (mkName (techInvalidTypeName (head values))))) []
                : [ Clause
                        [LitP (IntegerL (fromIntegral (techId u)))]
                        (NormalB (ConE (techName u)))
                        []
                  | u <- values
                  ]
                ++ [Clause [WildP] (NormalB (ConE (mkName (techInvalidTypeName (head values))))) []]
        ]

genTraitFunction :: (String, String) -> [ParsedUnit] -> Dec
genTraitFunction (traitName, traitFunc) units =
    let matchClauses =
            [ Clause
                [ConP (unitName u) [] []]
                (NormalB (ConE (if HashMap.lookup traitName (traits u) == Just True then 'True else 'False)))
                []
            | u <- units
            ]
        fallback = Clause [WildP] (NormalB (ConE 'False)) []
     in FunD (mkName traitFunc) (matchClauses ++ [fallback])

generateTechTypeStuff :: Q [Dec]
generateTechTypeStuff = do
    addDependentFile jsonDataFileName
    content <- runIO $ B.readFile jsonDataFileName
    let Just val = decode content
    parsedUnits <- parseUnits val
    parsedAbs <- parseAbilities val

    let dataDeclUnitTypeId = genDataDecl parsedUnits
        enumDeclUnitTypeId = genEnumInstance parsedUnits
        traitDecls =
            map
                (\trait -> genTraitFunction trait parsedUnits)
                [ ("is_structure", "isUnitStructure")
                , ("is_worker", "isUnitWorker")
                , ("is_townhall", "isUnitTownhall")
                , ("is_addon", "isUnitAddon")
                , ("needs_power", "isUnitNeedsPower")
                ]

        dataDeclAbilityId = genDataDecl parsedAbs
        enumDeclAbilityId = genEnumInstance parsedAbs

    return $ (dataDeclUnitTypeId : enumDeclUnitTypeId : traitDecls) ++ [dataDeclAbilityId, enumDeclAbilityId]

capitalize (c : cs) = toEnum (fromEnum c - 32) : cs
capitalize [] = []

sanitize :: Text -> String
sanitize = concatMap fix . splitWords . unpack
  where
    fix s@(c : cs)
        | all isAlphaNum s = toUpper c : cs
        | otherwise = "_" ++ s
    fix [] = []

-- crude word splitter
splitWords :: String -> [String]
splitWords = words . map (\c -> if c == '_' || c == '-' then ' ' else c)