module Utils where

import Debug.Trace

dbg = flip trace

triPartition :: (a -> Ordering) -> [a] -> ([a], [a], [a])
triPartition cmp = foldr partition ([], [], [])
  where
    partition x (less, eq, greater) =
      case cmp x of
        LT -> (x : less, eq, greater)
        EQ -> (less, x : eq, greater)
        GT -> (less, eq, x : greater)