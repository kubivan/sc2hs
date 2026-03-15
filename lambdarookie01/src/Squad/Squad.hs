module Squad.Squad (FSMSquad(..), squadId, replaceSquad) where

import Actions (UnitTag)
import Squad.Class

data FSMSquad s = Squad
    { squadUnits :: [UnitTag]
    , squadState :: s
    }

squadId :: FSMSquad s -> UnitTag
squadId s = head $ squadUnits s

replaceSquad :: FSMSquad s -> [FSMSquad s] -> [FSMSquad s]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)
