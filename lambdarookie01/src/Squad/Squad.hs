module Squad.Squad (FSMSquad (..), squadId, replaceSquad) where

import Actions (UnitTag)

data FSMSquad s = Squad
  { squadTags :: [UnitTag]
  , squadState :: s
  }

squadId :: FSMSquad s -> UnitTag
squadId s = head $ squadTags s

replaceSquad :: FSMSquad s -> [FSMSquad s] -> [FSMSquad s]
replaceSquad new = map (\s -> if squadId s == squadId new then new else s)
