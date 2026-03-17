module UnitAbilities where

import Data.HashMap.Strict qualified as HashMap
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId

type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId]
