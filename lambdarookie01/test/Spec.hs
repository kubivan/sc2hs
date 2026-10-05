module Main (main) where

import Test.Hspec (hspec)
import TestIntegrationRealGame (integrationRealGameTests)
import TestIntentDsl (intentDslTests)
import TestStepFlow (stepFlowTests)
import TestVisitedDecay (visitedDecayTests)

main :: IO ()
main = hspec $ do
  intentDslTests
  stepFlowTests
  visitedDecayTests
  integrationRealGameTests
