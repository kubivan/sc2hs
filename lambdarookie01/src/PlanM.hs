module PlanM where

import Actions
import AgentBulidUtils (agentUnitCost, canAfford, findBuilder, findFreeGeyser, findPlacementPos, pylonRadius)
import BotDynamicState (HasBuildIntents, HasReservedCost, agentGetBuildIntents, agentModifyBuildIntents, agentModifyReservedCost)
import Conduit (filterC, (.|))
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.HashMap.Strict qualified as HashMap
import Debug.Trace (traceM)
import Footprint (getFootprint)
import Intent
import Lens.Micro ((&), (.~), (^.))
import Observation
import SC2.Geometry (Pointable, distSquared, fromTuple)
import SC2.Grid
import SC2.Ids.AbilityId (AbilityId, isBuildAbility)
import SC2.Ids.Ids
import SC2.TechTree (unitToAbility)
import Squad (Target (..))
import StepMonad
import Units (Unit, mapTilePosC, runC, unitTypeC)
import Utils

data BOStep
  = BOTrain UnitTypeId
  | BOBuild UnitTypeId
  deriving (Show)

data BuildOrder = BuildOrder
  { boSteps :: Vector BOStep
  , boIndex :: Int
  }

boCurrentStep :: BuildOrder -> Maybe BOStep
boCurrentStep bo =
  boSteps bo !? boIndex bo


runBO :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => BuildOrder -> StepMonad d BuildOrder
runBO [] = pure []
runBO (u : us) =
  case boCurrentStep bo of
    Nothing -> pure bo
    Just boStep -> do
      let boIID = "bo-" ++ show boStep ++ show (boIndex boStep)
      exists <- intentExists boIID
      if exists then 
        pure bo
      else do
        spawnBoIntent boIID boStep

      spawned <- trySpawnBoIntent boIID boStep

trySpawnBoIntent
  :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d)
  => BOStep
  -> StepMonad d Bool
trySpawnBoIntent step = let boIID = "bo-" ++ show boStep ++ show (boIndex boStep) in
  case step of

    BOTrain ut -> do
      exists <- intentExists boIID

      if exists
        then pure False
        else do
          spawnIntent (IntentId boIID) (ensureUnit ut)
          pure True

    BOBuild ut pos -> do
      exists <- intentExists boIID
      if exists
        then pure False
        else do
          spawnIntent (IntentId boIID) (ensureStructure ut)
          pure True



tryCreate uid = runMaybeT $ createAction uid

createAction :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => UnitTypeId -> MaybeStepMonad d ()
createAction order = buildAction order -- <|> pylonBuildAction

