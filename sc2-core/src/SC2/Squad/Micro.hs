module SC2.Squad.Micro
  ( UnitMicro(..)
  , chooseMicroAction
  ) where

import SC2.Units.CombatEval (effectiveHp, unitDps, unitRange)
import Units (Unit)
import StepMonad (StepMonad, AgentDynamicState)

-- | Per-unit micro behavior kind (placeholder)
data UnitMicro
  = MicroFocusFire
  | MicroKite
  | MicroRetreat
  | MicroHold
  deriving (Eq, Show)

-- | Decide a micro action for a single unit. This is a stub that
-- will be extended: it currently picks actions based on simple
-- thresholds from CombatEval.
chooseMicroAction :: AgentDynamicState d => Unit -> StepMonad d UnitMicro
chooseMicroAction u = do
  let ehp = effectiveHp u
  dps <- unitDps u
  rng <- unitRange u
  if ehp < 30 then
    return MicroRetreat
  else if rng > 6 && dps > 8 then
    return MicroKite
  else
    return MicroFocusFire
