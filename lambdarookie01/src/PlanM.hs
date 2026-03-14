module PlanM where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Text (pack)
import Intent
import SC2.Ids.UnitTypeId (UnitTypeId)
import StepMonad

data BOStep
  = BOBuild UnitTypeId
  | BOTrain UnitTypeId
  deriving (Eq, Show)

type BuildOrder = [BOStep]

boFromUnits :: [UnitTypeId] -> BuildOrder
boFromUnits = map BOBuild

boCurrentStep :: BuildOrder -> Maybe BOStep
boCurrentStep [] = Nothing
boCurrentStep (step : _) = Just step

boIntentId :: IntentId
boIntentId = IntentId (pack "bo/current")

stepProgram :: (HasObs d, HasReservedCost d) => BOStep -> IntentDSL d ()
stepProgram (BOBuild uid) = ensureStructure uid
stepProgram (BOTrain uid) = ensureUnit uid

runBO :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => BuildOrder -> StepMonad d BuildOrder
runBO [] = pure []
runBO order@(step : rest) = do
  active <- intentExists boIntentId
  if active
    then do
      status <- stepIntent boIntentId
      case status of
        IntentCompleted -> pure rest
        IntentRunning -> pure order
        IntentFailed -> pure order
    else do
      spawnIntent boIntentId (stepProgram step)
      status <- stepIntent boIntentId
      case status of
        IntentCompleted -> pure rest
        IntentRunning -> pure order
        IntentFailed -> pure order

tryCreate :: (HasObs d, HasGrid d, HasReservedCost d) => UnitTypeId -> StepMonad d (Maybe ())
tryCreate uid = runMaybeT (createAction uid)

createAction :: (HasObs d, HasGrid d, HasReservedCost d) => UnitTypeId -> MaybeStepMonad d ()
createAction uid = do
  _ <- lift $ transientStep (ensureStructure uid)
  pure ()

tryTrain :: (HasObs d, HasGrid d, HasReservedCost d) => UnitTypeId -> StepMonad d ()
tryTrain uid = void (transientStep (ensureUnit uid))

