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

boToIntent :: (HasObs d, HasReservedCost d, HasGrid d) => BOStep -> IntentDSL d ()
boToIntent (BOBuild uid) = ensureStructure uid
boToIntent (BOTrain uid) = ensureUnit uid

runBO :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => BuildOrder -> StepMonad d BuildOrder
runBO [] = pure []
runBO order@(step : rest) = do
    frame <- (^. #gameLoop) <$> agentObs
    let boIntentId = IntentId ("bo-" ++ (show $ length order) ++ "-" ++ show step)
    active <- intentExists boIntentId
    traceM $ "[" <> show frame <> "][runBo][" <> (show boIntentId) <> "is_active: " <> (show active)
    if not active
        then do
            spawnIntent boIntentId (boToIntent step)
            pure order
        else
            pure rest
