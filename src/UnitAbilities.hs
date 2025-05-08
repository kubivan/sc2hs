module UnitAbilities where

import qualified Data.HashMap.Strict as HashMap
import qualified AbilityId
import UnitTypeId

type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId.AbilityId]