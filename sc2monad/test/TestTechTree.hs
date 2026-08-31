{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestTechTree (techTreeUnitTests) where

import Control.Monad.State.Strict (State (..), execState, get, modify')
import Data.HashMap.Strict qualified as HashMap
import SC2.Ids.Deps
import SC2.Ids.Ids
import SC2.Ids.UnitTypeId
import SC2.Ids.UpgradeId
import SC2.TechTree
import Test.Hspec

import SC2.Ids.AbilityId (AbilityId (..))
import SC2.Ids.UnitTypeId (UnitTypeId (..))
import SC2.Ids.UpgradeId (UpgradeId (..))

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State (..))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.List (foldl')
import Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import Lens.Micro ((^..), (^?), (^?!))

hasNoRepeatedTech :: [Tech] -> Bool
hasNoRepeatedTech xs = length xs == length (foldl' step [] xs)
 where
  step acc x = if x `elem` acc then acc else acc ++ [x]

techTreeUnitTests :: Spec
techTreeUnitTests = do
  describe "TechTree" $ do
    it "unit lookup" $ do
      -- res@(techPaths, abilityProducerPairs, researchPairs, trainPairs) <-
      --   generateTechPathesT "/home/ikubariev/src/sc2-workspace/sc2hs/sc2api/data/data.json"
      print "=========================================="
      print techPath
      print "=========================================="
      let carrierDeps = HashMap.lookup (TechUnit ProtossCarrier) techPath
      print carrierDeps
      carrierDeps
        `shouldBe` Just
          [ TechUnit ProtossPylon
          , TechUnit ProtossGateway
          , TechUnit ProtossCyberneticsCore
          , TechUnit ProtossStargate
          , TechUnit ProtossFleetBeacon
          , TechUnit ProtossCarrier
          ]

    it "upgrade lookup" $ do
      let blinkDeps = HashMap.lookup (TechUpgrade Blinktech) techPath
      blinkDeps
        `shouldBe` Just
          [ TechUnit ProtossPylon
          , TechUnit ProtossGateway
          , TechUnit ProtossCyberneticsCore
          , TechUnit ProtossTwilightCouncil
          , TechUpgrade Blinktech
          ]

    it "morph cycle is broken" $ do
      let infestorPath = HashMap.lookup (TechUnit ZergInfestorTerran) techPath
          burrowedPath = HashMap.lookup (TechUnit ZergInfestorTerranBurrowed) techPath
      hasNoRepeatedTech <$> infestorPath `shouldBe` Just True
      hasNoRepeatedTech <$> burrowedPath `shouldBe` Just True
      last <$> infestorPath `shouldBe` Just (TechUnit ZergInfestorTerran)
      last <$> burrowedPath `shouldBe` Just (TechUnit ZergInfestorTerranBurrowed)

  describe "TechTree height test" $ do
    it "height first tier" $ do
      let nexusHeight = length <$> HashMap.lookup (TechUnit ProtossNexus) techPath
          ccHeight = length <$> HashMap.lookup (TechUnit TerranCommandCenter) techPath
          hatcheryHeight = length <$> HashMap.lookup (TechUnit ZergHatchery) techPath
      nexusHeight `shouldBe` Just 1
      nexusHeight `shouldBe` ccHeight
      nexusHeight `shouldBe` hatcheryHeight
    it "height second tier" $ do
      let gate = length <$> HashMap.lookup (TechUnit ProtossGateway) techPath
          barrack = length <$> HashMap.lookup (TechUnit TerranBarracks) techPath
          pool = length <$> HashMap.lookup (TechUnit ZergSpawningPool) techPath
      gate `shouldBe` Just 2
      gate `shouldBe` barrack
      barrack `shouldBe` pool
