module ResourceFlowSM (updateResourceRateSM) where

import BotDynamicState (BotDynamicState (..))
import Data.Maybe (mapMaybe)
import StepMonad (StepMonad, agentModify, agentObs, agentStatic)

import Actions (Action)
import AgentBulidUtils (actionCostSafe)
import Lens.Micro ((^.))
import Observation (obsResources)
import ResourceFlow (resourceRateWindow, updateResourceRate)

-- calculateResourceRate :: Seq ResourceSample -> ResourceRate

updateResourceRateSM :: [Action] -> StepMonad BotDynamicState ()
updateResourceRateSM actions = do
  si <- agentStatic
  obs <- agentObs
  let frameCost = sum $ mapMaybe (actionCostSafe si) actions
  agentModify
    ( \ds ->
        ds
          { dsResourceRateState =
              updateResourceRate
                (fromIntegral $ obs ^. #gameLoop)
                (obsResources obs)
                frameCost
                resourceRateWindow
                (dsResourceRateState ds)
          }
    )
