module ResourceFlowSM (updateResourceRateSM, unitCostRate) where

import BotDynamicState (BotDynamicState (..))
import Data.Maybe (mapMaybe)
import StepMonad (StepMonad, agentModify, agentObs, agentStatic, unitTraits)

import Actions (Action)
import AgentBulidUtils (actionCostSafe)
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as HashMap
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Observation (obsResources)
import Proto.S2clientprotocol.Data_Fields qualified as Proto
import ResourceFlow (CostRate (CostRate), resourceRateWindow, updateResourceRate)
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (UnitTypeData (..))
import SC2.Proto.Data qualified as Proto
import SC2.TechTreeSM
import StepMonadUtils (withStatic)
import Units (Unit, fromEnum', toEnum', unitTypeId)

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

unitCostRate :: UnitTypeId -> StepMonad BotDynamicState CostRate
unitCostRate uid = do
  traits <- withStatic (\si -> unitTraits si HashMap.! uid)
  let buildTime = realToFrac $ traits ^. #buildTime
      minCost = fromIntegral . view #mineralCost $ traits
      gasCost = fromIntegral . view #vespeneCost $ traits
  pure $ CostRate (minCost / buildTime) (gasCost / buildTime)
