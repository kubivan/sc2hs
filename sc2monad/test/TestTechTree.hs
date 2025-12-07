module TestTechTree (techTreeUnitTests) where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import SC2.Ids.UnitTypeId
import SC2.Ids.UpgradeId
import SC2.TechTree
import Test.Hspec

printDeps :: IO ()
printDeps = do
    -- print trainDeps
    -- print "=================================================================================="
    -- print morphDeps
    -- print "=================================================================================="
    -- print buildDeps
    -- print "=================================================================================="
    -- print researchDeps
    -- print "=================================================================================="
    -- print unitAbilitiesDeps
    -- print "=================================================================================="
    -- print techDeps
    -- print "=================================================================================="
    print techPath

techTreeUnitTests :: Spec
techTreeUnitTests = do
        describe "TechTree" $ do
            it "unit lookup" $ do
                let carrierDeps = techPath HashMap.! TechUnit ProtossCarrier
                carrierDeps
                    `shouldBe` [ TechUnit ProtossNexus
                               , TechUnit ProtossProbe
                               , TechUnit ProtossPylon
                               , TechUnit ProtossGateway
                               , TechUnit ProtossCyberneticsCore
                               , TechUnit ProtossStargate
                               , TechUnit ProtossFleetBeacon
                               , TechUnit ProtossCarrier
                               ]
            it "upgrade lookup" $ do
                printDeps
                let blinkDeps = techPath HashMap.! TechUpgrade Blinktech

                blinkDeps
                    `shouldBe` [ TechUnit ProtossNexus
                               , TechUnit ProtossProbe
                               , TechUnit ProtossPylon
                               , TechUnit ProtossGateway
                               , TechUnit ProtossCyberneticsCore
                               , TechUnit ProtossTwilightCouncil
                               , TechUpgrade Blinktech
                               ]