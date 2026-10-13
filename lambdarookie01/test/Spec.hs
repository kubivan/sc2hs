module Main (main) where

import Test.Hspec (hspec)
import TestIntegrationRealGame (integrationRealGameTests)
import TestIntentDsl (intentDslTests)
import TestIstarMetrics (istarMetricTests)
import TestStepFlow (stepFlowTests)
import TestVisitedDecay (visitedDecayTests)
import TestVisionDecay (visionDecayTests)

main :: IO ()
main = hspec $ do
  intentDslTests
  istarMetricTests
  stepFlowTests
  visitedDecayTests
  visionDecayTests
  integrationRealGameTests
