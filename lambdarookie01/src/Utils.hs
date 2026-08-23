module Utils
  ( unitHasOrder
  , foodUsed
  , isFullLimit
  , unitIsHarvesting
  , getTargetUnitTag
  , unitIsAssignedTo
  , unitIsAssignedToAny
  , unitIsVespeneHarvester
  ) where

import Actions (UnitTag)
import Data.Maybe (listToMaybe, mapMaybe)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Proto.S2clientprotocol.Raw qualified as R
import SC2.Ids.Ids (AbilityId (..))
import SC2.Ids.UnitTypeId (UnitTypeId (..))
import SC2.Proto.Data (UnitOrder)
import SC2.Spatial qualified as Spatial
import StepMonad (HasObs, StepMonad)
import StepMonadUtils (withObs)
import Units (Unit, isAssimilator, isMineral, toEnum')

unitHasOrder :: AbilityId -> Unit -> Bool
unitHasOrder order u = order `elem` orders
 where
  orders = toEnum' . view #abilityId <$> u ^. #orders

foodUsed :: (HasObs d) => StepMonad d Int
foodUsed = withObs (fromIntegral . view (#playerCommon . #foodUsed))

isFullLimit :: (HasObs d) => StepMonad d Bool
isFullLimit = (==) 200 <$> foodUsed

unitIsHarvesting :: Units.Unit -> Bool
unitIsHarvesting u = orders == Just HARVESTGATHERPROBE || orders == Just HARVESTRETURNPROBE
 where
  orders = listToMaybe $ toEnum' . view #abilityId <$> u ^. #orders

getTargetUnitTag :: UnitOrder -> Maybe UnitTag
getTargetUnitTag unitOrder = case unitOrder ^. #maybe'target of
  Just (R.UnitOrder'TargetUnitTag tag) -> Just tag
  _ -> Nothing

unitIsAssignedTo :: Units.Unit -> Units.Unit -> Bool
unitIsAssignedTo building unit
  | isAssimilator building || isMineral building = building ^. #tag `elem` targets
  | toEnum' (building ^. #unitType) == ProtossNexus =
      unitIsHarvesting unit && closeEnough && withoutVespene
  | otherwise = error ("not implemented unitIsAssignedTo: " ++ show building)
 where
  targets = mapMaybe getTargetUnitTag (unit ^. #orders)
  closeEnough = Spatial.distManhattan building unit <= 12
  withoutVespene = unit ^. #vespeneContents == 0

unitIsAssignedToAny :: [Units.Unit] -> Units.Unit -> Bool
unitIsAssignedToAny buildings unit = any (`unitIsAssignedTo` unit) buildings

-- TODO: maybe the vespen & return check is enough
-- (probably units inside assimilators is not presented in the obs)
-- TODO: check if so: probes count between loops
unitIsVespeneHarvester :: [Units.Unit] -> Units.Unit -> Bool
unitIsVespeneHarvester assimilators u = unitIsAssignedToAny assimilators u || isReturnsVespene
 where
  orders = toEnum' . view #abilityId <$> u ^. #orders
  isReturnsVespene = listToMaybe orders == Just HARVESTRETURNPROBE && u ^. #vespeneContents > 0
