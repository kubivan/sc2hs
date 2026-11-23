{-# LANGUAGE OverloadedLabels #-}

module TestIntentDsl (intentDslTests) where

import AgentBulidUtils (findPlacementTargetNear)
import BotDynamicState (BotDynamicState)
import Data.HashMap.Strict qualified as HashMap
import Intent
import Observation (Cost (Cost))
import SC2.Grid (addMark, gridFromLines)
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossGateway, ProtossProbe, ProtossPylon))
import SC2.Proto.Data (Alliance (Self))
import Footprint (getFootprint)
import Lens.Micro ((&), (.~))
import SC2.Spatial (distSquaredI)
import SC2.TilePos (TilePos)
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

  it "andThen waits until the first structure is complete" $ do
    let rt =
          IntentRuntime
            { intentId = IntentId "andThen-waits-for-completion"
            , intentProgram =
                andThen
                  (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                  (PBuildStructure ProtossPylon (BSMonitoring 2 (TargetPos (4, 4))))
            , intentStartedFrame = 0
            }
        partialPylon = mockUnit 101 ProtossPylon Self (3, 3, 0) & #buildProgress .~ 0.5
        ds = mkDynamicState (mkObservation [partialPylon] 200) (gridFromLines ["          "])
        (runtime', status) = runIntentInTest ds rt

    status `shouldBe` IntentRunning
    case intentProgram runtime' of
      PAndThen (PBuildStructure ProtossPylon (BSMonitoring 1 _)) _ -> pure ()
      _ -> expectationFailure "the second branch should wait while the first is incomplete"

  it "andThenAfterStart passes the first structure's placement hint" $ do
    let rt =
          IntentRuntime
            { intentId = IntentId "andThen-after-start-placement"
            , intentProgram =
                andThenAfterStart
                  (PBuildStructure ProtossPylon (BSAccepted 101 (TargetPos (3, 3)) 0 0))
                  (PBuildStructureNear ProtossGateway (BSMonitoring 102 (TargetPos (4, 4))))
            , intentStartedFrame = 0
            }
        partialPylon = mockUnit 101 ProtossPylon Self (3, 3, 0) & #buildProgress .~ 0.5
        probe = mockUnit 102 ProtossProbe Self (1, 1, 0)
        grid = addMark (gridFromLines (replicate 12 "            ")) (getFootprint ProtossPylon) (3, 3)
        ds = mkDynamicState (mkObservation [partialPylon, probe] 200) grid
        staticInfo = mkStaticInfo (gridFromLines (replicate 12 "            "))
        (placement, _, _) = runStepM staticInfo HashMap.empty ds (findPlacementTargetNear ProtossGateway (3, 3))
        (runtime', status) = runIntentInTest ds rt

    case placement of
      Just (TargetPos pos) -> distSquaredI pos ((3, 3) :: TilePos) `shouldSatisfy` (<= 6 ^ (2 :: Int))
      _ -> expectationFailure "a valid placement should be found near the supplied anchor"
    status `shouldBe` IntentRunning
    case intentProgram runtime' of
      PWithPlacementHint (3, 3) (PBuildStructureNear ProtossGateway (BSMonitoring _ _)) -> pure ()
      _ -> expectationFailure "the second branch should use the first structure's placement as its hint"

  it "andThen keeps the active branch when it fails in the same frame" $ do
    let rt =
          IntentRuntime
            { intentId = IntentId "andThen-active-failure"
            , intentProgram =
                andThen
                  (PBuildStructure ProtossPylon (BSMonitoring 1 (TargetPos (3, 3))))
                  (PBuildStructure ProtossPylon (BSAccepted 777 (TargetPos (4, 4)) 0 0))
            , intentStartedFrame = 0
            }
        pylon = mockUnit 101 ProtossPylon Self (3, 3, 0)
        ds = mkDynamicState (mkObservation [pylon] 200) (gridFromLines ["          "])
        (runtime', status) = runIntentInTest ds rt

    status `shouldBe` IntentFailed
    case intentProgram runtime' of
      PWithPlacementHint _ (PBuildStructure ProtossPylon (BSAccepted 777 _ _ _)) -> pure ()
      _ -> expectationFailure "failure should retain the active second branch for cleanup"

  it "andThen fails fast when first branch fails" $ do
    let rt =
          IntentRuntime
            { intentId = IntentId "andThen-fail-fast"
            , intentProgram =
                andThen
                  (PBuildStructure ProtossPylon (BSAccepted 777 (TargetPos (20, 20)) 0 0))
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
                  (PBuildStructure ProtossPylon (BSAccepted 777 (TargetPos (20, 20)) 0 0))
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

runIntentInTest ::
  BotDynamicState -> IntentRuntime BotDynamicState -> (IntentRuntime BotDynamicState, IntentStatus)
runIntentInTest ds rt =
  let staticInfo = mkStaticInfo (gridFromLines (replicate 12 "            "))
   in fst3 (runStepM staticInfo HashMap.empty ds (runIntent rt))

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
