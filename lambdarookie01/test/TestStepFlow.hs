{-# LANGUAGE OverloadedLabels #-}

module TestStepFlow (stepFlowTests) where

import Actions (Action (..))
import Agent (StepPlan (..))
import BotDynamicState (BotDynamicState (..))
import Data.HashMap.Strict qualified as HashMap
import Data.ProtoLens (defMessage)
import Lens.Micro ((&), (.~))
import Observation (Cost (..))
import SC2.Grid (gridFromLines)
import SC2.Ids.AbilityId (AbilityId (NEXUSTRAINPROBE))
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossNexus, ProtossPylon))
import SC2.Proto.Data qualified as Proto
import SC2.Proto.Data (Alliance (Self))
import StepMonad (StaticInfo (..), runStepM)
import StepMonadUtils (agentCanAfford, agentCanAffordWith)
import Test.Hspec
import TestBot (BotPhase (..), agentStepPhase)
import TestFixtures (mkDynamicState, mkObservation, mkStaticInfo, mockUnit)
import Units (fromEnum')

stepFlowTests :: Spec
stepFlowTests = describe "Step flow (mocked)" $ do
    it "Opening schedules probe training and transitions into BuildOrderExecutor" $ do
        let grid = gridFromLines ["          ", "          ", "          "]
            nexus = mockUnit 1 ProtossNexus Self (3, 3, 0)
            obs0 = mkObservation [nexus] 0
            ds0 = mkDynamicState obs0 grid
            staticInfo = mkStaticInfo grid
            abilities = HashMap.empty
            (phase', StepPlan cmds _ _, _) = runStepM staticInfo abilities ds0 (agentStepPhase Opening)

        phaseTag phase' `shouldBe` "BuildOrderExecutor"
        cmds `shouldSatisfy` any isProbeTrain

    it "agentCanAfford applies global reserved penalty" $ do
        let grid = gridFromLines ["          "]
            pylonData :: Proto.UnitTypeData
            pylonData =
                defMessage
                    & #unitId .~ fromIntegral (fromEnum' ProtossPylon)
                    & #mineralCost .~ 80
                    & #vespeneCost .~ 0
            staticInfo = (mkStaticInfo grid){unitTraits = HashMap.fromList [(ProtossPylon, pylonData)]}
            obs =
                mkObservation [] 0
                    & #playerCommon . #minerals .~ 100
                    & #playerCommon . #vespene .~ 0
            ds0 = (mkDynamicState obs grid){dsReservedCost = Cost (-25) 0}
            (result, _, _) = runStepM staticInfo HashMap.empty ds0 (agentCanAfford ProtossPylon)

        result `shouldBe` False

    it "intent-local reserve offsets global reserved penalty" $ do
        let grid = gridFromLines ["          "]
            pylonData :: Proto.UnitTypeData
            pylonData =
                defMessage
                    & #unitId .~ fromIntegral (fromEnum' ProtossPylon)
                    & #mineralCost .~ 80
                    & #vespeneCost .~ 0
            staticInfo = (mkStaticInfo grid){unitTraits = HashMap.fromList [(ProtossPylon, pylonData)]}
            obs =
                mkObservation [] 0
                    & #playerCommon . #minerals .~ 100
                    & #playerCommon . #vespene .~ 0
            ds0 = (mkDynamicState obs grid){dsReservedCost = Cost (-25) 0}
            (result, _, _) = runStepM staticInfo HashMap.empty ds0 (agentCanAffordWith (Cost 20 0) ProtossPylon)

        result `shouldBe` True

phaseTag :: BotPhase -> String
phaseTag Opening = "Opening"
phaseTag BuildOrderExecutor{} = "BuildOrderExecutor"
phaseTag BuildArmyAndWin{} = "BuildArmyAndWin"

isProbeTrain :: Action -> Bool
isProbeTrain (SelfCommand ability _) = ability == NEXUSTRAINPROBE
isProbeTrain _ = False
