{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SC2.Ids.UnitTypeGen (generateUnitTypeStuff) where

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

data ParsedUnit = ParsedUnit
    { unitId :: UnitId
    , unitName :: UnitName
    , unitRace :: Race
    , traits :: HashMap String Bool
    }

parseUnits :: Value -> Q [ParsedUnit]
parseUnits val = do
    let Just (Array units) = val ^? key "Unit" -- Adjusted to match SC2 layout
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

genDataDecl :: [ParsedUnit] -> Dec
genDataDecl units =
    DataD
        []
        (mkName "UnitTypeId")
        []
        Nothing
        (NormalC invalidCon [] : map (\u -> NormalC (unitName u) []) units)
        [DerivClause Nothing (map ConT [''Eq, ''Ord, ''Show, ''Lift, ''Read])]
  where
    invalidCon = mkName "Invalid"

genEnumInstance :: [ParsedUnit] -> Dec
genEnumInstance units =
    InstanceD
        Nothing
        []
        (AppT (ConT ''Enum) (ConT (mkName "UnitTypeId")))
        [ FunD
            'fromEnum
            ( Clause [ConP (mkName "Invalid") [] []] (NormalB (LitE (IntegerL 0))) []
                : [ Clause
                        [ConP (unitName u) [] []]
                        (NormalB (LitE (IntegerL (fromIntegral (unitId u)))))
                        []
                  | u <- units
                  ]
            )
        , FunD 'toEnum $
            Clause [LitP (IntegerL 0)] (NormalB (ConE (mkName "Invalid"))) []
                : [ Clause
                        [LitP (IntegerL (fromIntegral (unitId u)))]
                        (NormalB (ConE (unitName u)))
                        []
                  | u <- units
                  ]
                ++ [Clause [WildP] (NormalB (ConE (mkName "Invalid"))) []]
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

generateUnitTypeStuff :: Q [Dec]
generateUnitTypeStuff = do
    addDependentFile jsonDataFileName
    content <- runIO $ B.readFile jsonDataFileName
    let Just val = decode content
    parsed <- parseUnits val

    let dataDecl = genDataDecl parsed
        enumDecl = genEnumInstance parsed
        traitDecls =
            map
                (\trait -> genTraitFunction trait parsed)
                [ ("is_structure", "isUnitStructure")
                , ("is_worker", "isUnitWorker")
                , ("is_townhall", "isUnitTownhall")
                , ("is_addon", "isUnitAddon")
                , ("needs_power", "isUnitNeedsPower")
                ]

    return (dataDecl : enumDecl : traitDecls)

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