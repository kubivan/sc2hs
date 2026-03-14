{-# LANGUAGE OverloadedLabels #-}

module TestStepFlow (stepFlowTests) where

import Actions (Action (..))
import Agent (StepPlan (..))
import BotDynamicState (BotDynamicState (..))
import Data.HashMap.Strict qualified as HashMap
import SC2.Grid (gridFromLines)
import SC2.Ids.AbilityId (AbilityId (NEXUSTRAINPROBE))
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossNexus))
import SC2.Proto.Data (Alliance (Self))
import StepMonad (runStepM)
import Test.Hspec
import TestBot (BotPhase (..), agentStepPhase)
import TestFixtures (mkDynamicState, mkObservation, mkStaticInfo, mockUnit)

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

phaseTag :: BotPhase -> String
phaseTag Opening = "Opening"
phaseTag BuildOrderExecutor{} = "BuildOrderExecutor"
phaseTag BuildArmyAndWin{} = "BuildArmyAndWin"

isProbeTrain :: Action -> Bool
isProbeTrain (SelfCommand ability _) = ability == NEXUSTRAINPROBE
isProbeTrain _ = False
