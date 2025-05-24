
module SC2.Squad.FSSquadIdle where

import SC2.Squad.Squad

import Debug.Trace (traceM)

data FSSquadIdle = FSSquadIdle

instance SquadFS FSSquadIdle where

    fsStep s _ = traceM "[step] idle" -- >> TODO: wanderAround 5
    fsUpdate _ st = pure (True, st)
    fsOnEnter s _ = traceM $ "[enter] Idle " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] Idle " ++ show (squadId s)