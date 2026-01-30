{-# LANGUAGE ImportQualifiedPost #-}

-- | Helper module for strategy detection initialization and updates
module StrategyDetectionIntegration where

import SC2.Army.StrategyDetection (StrategyBelief, emptyBelief, updateWithEvent, DiagnosticEvent)

-- | Initialize bot state with strategy detection
initBotStateWithStrategy :: StrategyBelief
initBotStateWithStrategy = emptyBelief

-- | Update strategy belief after observing diagnostic event
updateBotStrategy :: StrategyBelief -> DiagnosticEvent -> StrategyBelief
updateBotStrategy belief ev = updateWithEvent ev belief
