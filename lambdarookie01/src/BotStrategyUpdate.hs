{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

module BotStrategyUpdate where

import DiagnosticExtraction (extractDiagnosticEvents)
import Observation
import SC2.Army.StrategyDetection (
    StrategyBelief, 
    emptyBelief, 
    updateWithEvent,
    recognizedStrategy,
    allPosteriors,
    diagnosticTrace
  )
import StrategyDetectionIntegration (initBotStateWithStrategy, updateBotStrategy)
import StepMonad
import BotDynamicState

import Debug.Trace (traceM)
import Control.Monad (when)
import Lens.Micro ((^.))

-- | Initialize strategy belief at bot startup
initializeStrategyBelief :: StrategyBelief
initializeStrategyBelief = initBotStateWithStrategy

-- | Update strategy belief with events observed this frame
updateStrategyBeliefThisFrame ::
  StrategyBelief ->
  Observation ->
  Observation ->
  StepMonad BotDynamicState StrategyBelief
updateStrategyBeliefThisFrame belief obs obsPrev = do
  let events = extractDiagnosticEvents obs obsPrev
      belief' = foldl (\b ev -> updateBotStrategy b ev) belief events
  
  -- Optional: log current belief state every 200 frames
  when (obs ^. #gameLoop `mod` 200 == 0 && not (null events)) $
    traceM $ "Strategy belief updated: events=" ++ show events ++
             ", recognized=" ++ show (recognizedStrategy belief') ++
             ", posteriors=" ++ show (allPosteriors belief')
  
  return belief'

-- | Log strategy belief periodically
logStrategyBeliefSummary :: StrategyBelief -> Observation -> StepMonad BotDynamicState ()
logStrategyBeliefSummary belief obs = do
  when (obs ^. #gameLoop `mod` 500 == 0) $ do
    let recognized = recognizedStrategy belief
        posteriors = allPosteriors belief
        trace = diagnosticTrace belief
    traceM $ "=== Strategy Summary (frame " ++ show (obs ^. #gameLoop) ++ ") ==="
    traceM $ "  Recognized: " ++ show recognized
    traceM $ "  All Posteriors: " ++ show posteriors
    traceM $ "  Trace entries: " ++ show (length trace)
