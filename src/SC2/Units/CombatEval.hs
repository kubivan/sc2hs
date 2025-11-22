{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module SC2.Units.CombatEval (
    effectiveHp,
    unitDps,
    unitRange,
) where

import Lens.Micro
import Lens.Micro.Extras (view)
import SC2.Proto.Data (Unit, UnitTypeData)
import StepMonad
import StepMonadUtils (siUnitData)

-- | Effective HP ignoring armor/shield regen for now.
effectiveHp :: Unit -> Float
effectiveHp u = (u ^. #health) + (u ^. #shield)

-- | Aggregate DPS approximation: sum over weapons (damage * attacks / speed).
-- Speed is time between attacks, so DPS contribution is (damage * attacks)/speed.
unitDps :: AgentDynamicState d => Unit -> StepMonad d Float
unitDps u = do
    udata <- siUnitData u
    let ws = udata ^. #weapons
        weaponDps w =
            let dmg     = w ^. #damage
                attacks = fromIntegral (w ^. #attacks)
                speed   = w ^. #speed
            in if speed <= 0 then 0 else dmg * attacks / speed
    return (sum (map weaponDps ws))

-- | Primary weapon range (first weapon) fallback 0.
unitRange :: AgentDynamicState d => Unit -> StepMonad d Float
unitRange u = do
    udata <- siUnitData u
    return $ case udata ^. #weapons of
        [] -> 0
        (w:_) -> w ^. #range

-- | Simple ground-hit capability: does any weapon target Ground or Any.
