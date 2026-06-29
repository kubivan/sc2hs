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

buildOrderIntentId :: IntentId
buildOrderIntentId = IntentId "bo-root"

boFromUnits :: [UnitTypeId] -> BuildOrder
boFromUnits = map BOBuild

boToIntent :: BOStep -> IntentProgram d
boToIntent (BOBuild uid) = buildStructureIntent uid
boToIntent (BOTrain uid) = trainUnitIntent uid

programToBoStep :: IntentProgram d -> Maybe BOStep
programToBoStep intent =
    case intent of
        PBuildStructure uid _ -> Just (BOBuild uid)
        PTrainUnit uid _ -> Just (BOTrain uid)
        _ -> Nothing

programToBuildOrder :: IntentProgram d -> Maybe BuildOrder
programToBuildOrder intent =
    case intent of
        PAndThen left right -> (++) <$> programToBuildOrder left <*> programToBuildOrder right
        _ -> (: []) <$> programToBoStep intent

composeBuildOrderWith :: (BOStep -> IntentProgram d) -> BuildOrder -> Maybe (IntentProgram d)
composeBuildOrderWith toIntent steps =
    case map toIntent steps of
        [] -> Nothing
        intent : rest -> Just (foldl andThen intent rest)

boToComposite :: BuildOrder -> Maybe (IntentProgram d)
boToComposite = composeBuildOrderWith boToIntent

spawnBuildOrderIntent :: (HasObs d, HasBuildIntents d) => BuildOrder -> StepMonad d Bool
spawnBuildOrderIntent buildOrder =
    case boToComposite buildOrder of
        Nothing -> pure False
        Just program -> spawnIntent buildOrderIntentId program >> pure True

spawnCurrentStepIntent :: (HasObs d, HasBuildIntents d) => IntentId -> BOStep -> StepMonad d ()
spawnCurrentStepIntent iid step = spawnIntent iid (boToIntent step)

runBO :: (HasObs d, HasBuildIntents d) => IntentOutcomeStore -> BuildOrder -> StepMonad d BuildOrder
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
