module Squad.Squad (FSMSquad(..), squadId, Target(..), replaceSquad) where

import Actions (UnitTag)
import SC2.Grid.TilePos
import Squad.Class

data FSMSquad s = Squad
    { squadUnits :: [UnitTag]
    , squadState :: s
    }

squadId :: FSMSquad s -> UnitTag
squadId s = head $ squadUnits s

data Target
    = TargetPos TilePos
    | TargetUnit UnitTag
    deriving (Eq, Show)

replaceSquad :: FSMSquad s -> [FSMSquad s] -> [FSMSquad s]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)
