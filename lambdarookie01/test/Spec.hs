module Main (main) where

import Test.Hspec (hspec)
import TestIntegrationRealGame (integrationRealGameTests)
import TestIntentDsl (intentDslTests)
import TestStepFlow (stepFlowTests)

main :: IO ()
main = hspec $ do
  intentDslTests
  stepFlowTests
  integrationRealGameTests
