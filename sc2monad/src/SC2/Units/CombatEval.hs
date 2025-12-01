{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module SC2.Units.CombatEval (
    effectiveHp,
    unitDps,
    unitRange,
) where

import SC2.Proto.Data qualified as Proto
import StepMonad (StepMonad)
import Lens.Micro ((^.))

-- | Calculate effective HP considering shields
effectiveHp :: Proto.Unit -> Float
effectiveHp u = u ^. #health + u ^. #shield

-- | Calculate unit DPS (damage per second)
unitDps :: Monad m => Proto.Unit -> m Float
unitDps _u = return 0.0  -- TODO: implement based on unit type and weapon stats

-- | Get unit attack range
unitRange :: Monad m => Proto.Unit -> m Float  
unitRange _u = return 0.0  -- TODO: implement based on unit type
