
module PlanM where

import Actions
import AgentBulidUtils (canAfford, findBuilder, findFreeGeyser, findPlacementPos, pylonRadius, agentUnitCost)
import Conduit (filterC, (.|))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Writer.Strict (WriterT, listen, runWriterT, tell)
import Data.HashMap.Strict qualified as HashMap
import Debug.Trace (traceM)
import Footprint (getFootprint)
import Lens.Micro ((.~), (^.), (&))
import Observation
import SC2.Geometry (Pointable, distSquared, fromTuple)
import SC2.Grid
import SC2.Ids.AbilityId (isBuildAbility)
import SC2.Ids.Ids
import SC2.TechTree (unitToAbility)
import StepMonad
import Units (mapTilePosC, runC, unitTypeC)
import Utils
import Squad (Target)

-- import Control.Monad
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.State
-- import Control.Monad.Writer.Strict


data ActionCtx = ActionCtx
  { actCtxId :: AbilityId
  --, actCtxKind  :: ActionKind
  , actCtxExec :: UnitTag
  , actCtxTarget :: Target
  , actCtxState :: ActionState
  }

data ActionState
  = Planned
  | Issued
  | InProgress
  | Failed --ActionFailure


type BuildOrder = [UnitTypeId]

reserveResources :: (HasObs d) => UnitTypeId -> StepMonad d ()
reserveResources uid = do
    obs <- agentObs
    cost <- agentUnitCost uid
    let minerals = obs ^. #playerCommon . #minerals
        vespene  = obs ^. #playerCommon . #vespene
        obs' = obs
          & (#playerCommon . #minerals .~ (minerals - fromIntegral (mineralCost cost)))
          & (#playerCommon . #vespene .~ (vespene - fromIntegral (gasCost cost)))
    agentModifyObs (const obs')

reserveLand :: (HasGrid d) => UnitTypeId ->TilePos -> StepMonad d ()
reserveLand uid pos = do
  agentModifyGrid (\g -> addMark g (getFootprint uid) pos)

splitAffordable
  :: (HasObs d, HasGrid d)
  => BuildOrder
  -> StepMonad d BuildOrder
splitAffordable bo = runBO bo

runBO :: (HasObs d, HasGrid d) => BuildOrder -> StepMonad d BuildOrder
runBO [] = pure []
runBO (u:us) = do
  ok <- tryCreate u
  case ok of
    Nothing -> pure (u:us)
    Just _  -> runBO us

tryCreate :: (HasObs d, HasGrid d) => UnitTypeId -> StepMonad d (Maybe ())
tryCreate uid = runMaybeT $ createAction uid

createAction :: (HasObs d, HasGrid d) => UnitTypeId -> MaybeStepMonad d ()
createAction order = do
  buildAction order <|> pylonBuildAction

inBuildThechTree :: UnitTypeId -> StepMonad d Bool
inBuildThechTree uid = do
  abilities <- agentAbilities
  si        <- agentStatic
  let ability = unitToAbility (unitTraits si) uid
  return $ ability `elem` (abilities HashMap.! ProtossProbe)

distantEnough :: (Foldable t, Pointable p1, Pointable p2) => t p2 -> Float -> p1 -> Bool
distantEnough units radius pos = all (\p -> distSquared pos p >= radius * radius) units

guardStepM :: StepMonad d Bool -> MaybeStepMonad d ()
guardStepM action = lift action >>= guard

pylonBuildAction :: (HasObs d, HasGrid d) => MaybeStepMonad d ()
pylonBuildAction = do
  guardStepM (canAfford ProtossPylon)
  si      <- lift agentStatic
  obs     <- lift agentObs
  grid <- lift agentGrid
  let hasPylonsInProgress = not $ Prelude.null $ runC $
        unitsSelf obs .| unitTypeC ProtossPylon .| filterC (\u -> u ^. #buildProgress < 1)
  guard (not hasPylonsInProgress)
  builder <- MaybeT . return $ findBuilder obs
  let findPylonPlacement = findPlacementPoint grid (heightMap si) (getFootprint ProtossPylon) (tilePos (builder ^. #pos))
      pylonsPos          = runC $ unitsSelf obs .| unitTypeC ProtossPylon .| mapTilePosC
      pylonCriteria      = distantEnough pylonsPos
  pylonPos <- MaybeT . return $ findPylonPlacement (pylonCriteria pylonRadius)
  lift $ reserveLand ProtossPylon pylonPos
  lift $ reserveResources ProtossPylon
  lift $ command [PointCommand PROTOSSBUILDPYLON [builder] (fromTuple pylonPos)]
  -- `Utils.dbg` ("pylonPos: " ++ show pylonPos)

buildAction :: (HasObs d, HasGrid d) => UnitTypeId -> MaybeStepMonad d ()
buildAction ProtossAssimilator = do
  guardStepM (canAfford ProtossAssimilator)
  obs     <- lift agentObs
  builder <- MaybeT . return $ findBuilder obs
  geyser  <- MaybeT . return $ findFreeGeyser obs
  lift $ reserveResources ProtossAssimilator
  lift $ command [UnitCommand PROTOSSBUILDASSIMILATOR [builder] geyser]

buildAction order = do
  guardStepM (inBuildThechTree order)
  guardStepM (canAfford order)
  si      <- lift agentStatic
  obs     <- lift agentObs
  grid    <- lift agentGrid
  let ability   = unitToAbility (unitTraits si) order
  guard (isBuildAbility ability)
  builder <- MaybeT . return $ findBuilder obs
  pos     <- MaybeT . return $ findPlacementPos obs (expandsPos si) grid (heightMap si) order
  traceM $ show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!"
  lift $ reserveResources order
  lift $ reserveLand order pos
  lift $ command[PointCommand ability [builder] (fromTuple pos)]
