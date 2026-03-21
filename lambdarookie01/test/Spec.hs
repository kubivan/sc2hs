module Main (main) where

import Test.Hspec (hspec)
import TestIntentDsl (intentDslTests)
import TestIntegrationRealGame (integrationRealGameTests)
import TestStepFlow (stepFlowTests)

main :: IO ()
main = hspec $ do
    intentDslTests
    stepFlowTests
    integrationRealGameTests
