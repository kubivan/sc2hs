module TestTechTree (techTreeUnitTests) where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import SC2.Ids.UnitTypeId
import SC2.Ids.UpgradeId
import SC2.TechTree
import Test.Hspec

loadTestData :: IO TechDeps
loadTestData = do
    techDeps <- loadDeps "test/data/tech_deps.json"

    return $ fromJust techDeps

techTreeUnitTests :: Spec
techTreeUnitTests =
    beforeAll loadTestData $ do
        describe "TechTree" $ do
            it "unit lookup" $ \techDeps -> do
                let carrierDeps = techDeps HashMap.! TechUnit ProtossCarrier

                carrierDeps
                    `shouldBe` [ TechUnit ProtossNexus
                               , TechUnit ProtossGateway
                               , TechUnit ProtossCyberneticscore
                               , TechUnit ProtossStargate
                               , TechUnit ProtossFleetbeacon
                               , TechUnit ProtossCarrier
                               ]
            it "upgrade lookup" $ \techDeps -> do
                testDeps
                let blinkDeps = techDeps HashMap.! TechUpgrade Blinktech

                blinkDeps
                    `shouldBe` [ TechUnit ProtossNexus
                               , TechUnit ProtossGateway
                               , TechUnit ProtossCyberneticscore
                               , TechUnit ProtossTwilightcouncil
                               , TechUpgrade Blinktech
                               ]