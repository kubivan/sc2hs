
{-# LANGUAGE MultiParamTypeClasses #-}

module Squad.FSSquadIdle where

import Squad.Squad
import Squad.State

import Debug.Trace (traceM)

data FSSquadIdle = FSSquadIdle

instance SquadFS SquadState FSSquadIdle where

    fsStep s _ = traceM "[step] idle" -- >> TODO: wanderAround 5
    fsUpdate _ st = pure (False, st)
    fsOnEnter s _ = traceM $ "[enter] Idle " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] Idle " ++ show (squadId s)
    fsTransitionNext _ _ = pure $ wrapState FSSquadIdle