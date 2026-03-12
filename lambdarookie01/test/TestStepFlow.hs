{-# LANGUAGE OverloadedLabels #-}

module TestStepFlow (stepFlowTests) where

import Actions (Action (..))
import Agent (StepPlan (..))
import BotDynamicState (BotDynamicState (..))
import Data.HashMap.Strict qualified as HashMap
import Data.ProtoLens (defMessage)
import Footprint (getFootprint)
import Intent
import Observation (Cost (..))
import SC2.Grid (addMark, gridFromLines, removeMark)
import SC2.Ids.AbilityId (AbilityId (NEXUSTRAINPROBE, PROTOSSBUILDPYLON))
import SC2.Ids.UnitTypeId (UnitTypeId (NeutralMineralField, ProtossNexus, ProtossProbe, ProtossPylon))
import SC2.Proto.Data (Alliance (Neutral, Self))
import StepMonad (runStepM)
import Test.Hspec
import TestBot (BotPhase (..), agentStepPhase, processQueue, rollbackIntentFromActionError)
import TestFixtures (mkDynamicState, mkObservation, mkStaticInfo, mockUnit)

import Lens.Micro ((&), (.~))

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

    it "processQueue reports interrupted queue when intent is rolled back" $ do
        let grid = gridFromLines ["          ", "          ", "          "]
            probe = mockUnit 11 ProtossProbe Self (4, 4, 0)
            mineral = mockUnit 12 NeutralMineralField Neutral (5, 4, 0)
            obs0 = mkObservation [probe, mineral] 10
            ds0 = (mkDynamicState obs0 grid){dsBuildIntents = HashMap.singleton bid rolledBackIntent}
            bid = (11, PROTOSSBUILDPYLON)
            rolledBackIntent =
                BuildIntent
                    { biId = bid
                    , biExecutor = 11
                    , biAbility = PROTOSSBUILDPYLON
                    , biAction = SelfCommand PROTOSSBUILDPYLON [probe]
                    , biUnitType = ProtossPylon
                    , biReservedCost = Cost 100 0
                    , biGhostMarks = []
                    , biIssuedAtFrame = 9
                    , biState = IntentRolledBack
                    , biRollbackReason = Just RollbackActionError
                    }
            staticInfo = mkStaticInfo grid
            ((active, interrupted), _, _) = runStepM staticInfo HashMap.empty ds0 (processQueue [bid] ([], []))

        active `shouldBe` []
        interrupted `shouldBe` [ProtossPylon]

    it "rollbackIntentFromActionError removes mark and marks intent as rolled back" $ do
        let baseGrid = gridFromLines ["             ", "             ", "             ", "             "]
            probe = mockUnit 77 ProtossProbe Self (5, 2, 0)
            obs0 = mkObservation [probe] 20
            bid = (77, PROTOSSBUILDPYLON)
            markedPos = (2, 1)
            gridMarked = addMark baseGrid (getFootprint ProtossPylon) markedPos
            intent =
                BuildIntent
                    { biId = bid
                    , biExecutor = 77
                    , biAbility = PROTOSSBUILDPYLON
                    , biAction = SelfCommand PROTOSSBUILDPYLON [probe]
                    , biUnitType = ProtossPylon
                    , biReservedCost = Cost 100 0
                    , biGhostMarks = [GhostMarkRef ProtossPylon markedPos]
                    , biIssuedAtFrame = 20
                    , biState = IntentIssued
                    , biRollbackReason = Nothing
                    }
            ds0 = (mkDynamicState obs0 gridMarked){dsBuildIntents = HashMap.singleton bid intent}
            err =
                defMessage
                    & #maybe'unitTag .~ Just 77
                    & #maybe'abilityId .~ Just (fromIntegral $ fromEnum PROTOSSBUILDPYLON)
            ds1 = rollbackIntentFromActionError ds0 err
            updatedIntent = HashMap.lookup bid (dsBuildIntents ds1)

        fmap biState updatedIntent `shouldBe` Just IntentRolledBack
        fmap biRollbackReason updatedIntent `shouldBe` Just (Just RollbackActionError)
        removeMark (dsGrid ds0) (getFootprint ProtossPylon) markedPos `shouldBe` dsGrid ds1

phaseTag :: BotPhase -> String
phaseTag Opening = "Opening"
phaseTag BuildOrderExecutor{} = "BuildOrderExecutor"
phaseTag BuildArmyAndWin{} = "BuildArmyAndWin"

isProbeTrain :: Action -> Bool
isProbeTrain (SelfCommand ability _) = ability == NEXUSTRAINPROBE
isProbeTrain _ = False
