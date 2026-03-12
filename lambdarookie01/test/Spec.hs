module Main (main) where

import Test.Hspec (hspec)
import TestIntegrationRealGame (integrationRealGameTests)
import TestStepFlow (stepFlowTests)

main :: IO ()
main = hspec $ do
    stepFlowTests
    integrationRealGameTests
