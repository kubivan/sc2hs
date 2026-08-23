module SC2.TechTreeSM (abilityToUnitSM, abilityToUnitSafeSM, unitToAbilitySM) where

import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.TechTree
import StepMonad
import StepMonadUtils

abilityToUnitSM :: AbilityId -> StepMonad d UnitTypeId
abilityToUnitSM = withStatic . flip (abilityToUnit . unitTraits)

abilityToUnitSafeSM :: AbilityId -> StepMonad d (Maybe UnitTypeId)
abilityToUnitSafeSM = withStatic . flip (abilityToUnitSafe . unitTraits)

unitToAbilitySM :: UnitTypeId -> StepMonad d AbilityId
unitToAbilitySM = withStatic . flip (unitToAbility . unitTraits)
