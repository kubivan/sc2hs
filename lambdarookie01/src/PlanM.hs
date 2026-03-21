module PlanM where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Map qualified as Map
import Data.Text (pack)
import Debug.Trace (traceM)
import Intent
import Lens.Micro
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

boToIntent :: BOStep -> IntentProgram d
boToIntent (BOBuild uid) = buildStructureIntent uid
boToIntent (BOTrain uid) = trainUnitIntent uid

spawnCurrentStepIntent :: (HasObs d, HasBuildIntents d) => IntentId -> BOStep -> StepMonad d ()
spawnCurrentStepIntent iid step = spawnIntent iid (boToIntent step)

runBO :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => IntentOutcomeStore -> BuildOrder -> StepMonad d BuildOrder
runBO _ [] = pure []
runBO outcomes order@(step : rest) = do
    frame <- agentObs <&> (^. #gameLoop)
    let boIntentId = IntentId ("bo-" ++ (show $ length order) ++ "-" ++ show step)
    active <- intentExists boIntentId
    let status = Map.lookup boIntentId outcomes
    traceM $ "[" <> show frame <> "][runBo][" <> (show boIntentId) <> "] is_active: " <> (show active) <> " status: " <> show status
    if status == Just IntentCompleted
        then pure rest
        else
            if active
                then pure order
                else do
                    spawnCurrentStepIntent boIntentId step
                    pure order
