{-# LANGUAGE OverloadedLabels #-}

module TestIntentDsl (intentDslTests) where

import BotDynamicState (BotDynamicState)
import Data.HashMap.Strict qualified as HashMap
import Intent
import Observation (Cost (Cost))
import SC2.Grid (gridFromLines)
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossGateway, ProtossProbe, ProtossPylon))
import SC2.Proto.Data (Alliance (Self))
import StepMonad (runStepM)
import Target (Target (TargetPos))
import Test.Hspec
import TestFixtures (mkDynamicState, mkObservation, mkStaticInfo, mockUnit)

intentDslTests :: Spec
intentDslTests = describe "Intent DSL combinators" $ do
    it "andThen executes next intent after completion" $ do
        let rt =
                IntentRuntime
                    { intentId = IntentId "andThen-complete"
                    , intentProgram =
                        andThen
                            (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                            (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (4, 4))))
                    , intentStartedFrame = 0
                    }
            pylonA = mockUnit 101 ProtossPylon Self (3, 3, 0)
            pylonB = mockUnit 102 ProtossPylon Self (4, 4, 0)
            ds = mkDynamicState (mkObservation [pylonA, pylonB] 200) (gridFromLines ["          "])

        snd (runIntentInTest ds rt) `shouldBe` IntentCompleted

    it "andThen fails fast when first branch fails" $ do
        let rt =
                IntentRuntime
                    { intentId = IntentId "andThen-fail-fast"
                    , intentProgram =
                        andThen
                            (PBuildStructure ProtossPylon (BSAccepted 777 (TargetPos (20, 20)) 0))
                            (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                    , intentStartedFrame = 0
                    }
            pylon = mockUnit 101 ProtossPylon Self (3, 3, 0)
            ds = mkDynamicState (mkObservation [pylon] 200) (gridFromLines ["          "])

        snd (runIntentInTest ds rt) `shouldBe` IntentFailed

    it "orElse runs fallback when primary fails" $ do
        let rt =
                IntentRuntime
                    { intentId = IntentId "orElse-fallback"
                    , intentProgram =
                        orElse
                            (PBuildStructure ProtossPylon (BSAccepted 777 (TargetPos (20, 20)) 0))
                            (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                    , intentStartedFrame = 0
                    }
            pylon = mockUnit 101 ProtossPylon Self (3, 3, 0)
            ds = mkDynamicState (mkObservation [pylon] 200) (gridFromLines ["          "])

        snd (runIntentInTest ds rt) `shouldBe` IntentCompleted

    it "andThen propagates NeedPrerequisite from active branch" $ do
        let rt =
                IntentRuntime
                    { intentId = IntentId "andThen-need-propagation"
                    , intentProgram =
                        andThen
                            (PBuildStructure ProtossGateway (BSGathering (Cost 0 0)))
                            (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                    , intentStartedFrame = 0
                    }
            probe = mockUnit 301 ProtossProbe Self (2, 2, 0)
            ds = mkDynamicState (mkObservation [probe] 200) (gridFromLines ["          "])

        snd (runIntentInTest ds rt) `shouldBe` IntentNeedsPrerequisite ProtossPylon

    it "orElse keeps NeedPrerequisite instead of switching fallback" $ do
        let rt =
                IntentRuntime
                    { intentId = IntentId "orElse-need-propagation"
                    , intentProgram =
                        orElse
                            (PBuildStructure ProtossGateway (BSGathering (Cost 0 0)))
                            (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                    , intentStartedFrame = 0
                    }
            probe = mockUnit 302 ProtossProbe Self (2, 2, 0)
            ds = mkDynamicState (mkObservation [probe] 200) (gridFromLines ["          "])

        snd (runIntentInTest ds rt) `shouldBe` IntentNeedsPrerequisite ProtossPylon

    it "andThen remains running when first branch blocks" $ do
        let rt =
                IntentRuntime
                    { intentId = IntentId "andThen-running"
                    , intentProgram =
                        andThen
                            (PBuildStructure ProtossGateway (BSGathering (Cost 0 0)))
                            (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                    , intentStartedFrame = 0
                    }
            ds = mkDynamicState (mkObservation [] 200) (gridFromLines ["          "])

        snd (runIntentInTest ds rt) `shouldBe` IntentRunning

runIntentInTest :: BotDynamicState -> IntentRuntime BotDynamicState -> (IntentRuntime BotDynamicState, IntentStatus)
runIntentInTest ds rt =
    let staticInfo = mkStaticInfo (gridFromLines ["          "])
     in fst3 (runStepM staticInfo HashMap.empty ds (runIntent rt))

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x