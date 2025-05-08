module UnitAbilities where

import qualified Data.HashMap.Strict as HashMap
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId

type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId]