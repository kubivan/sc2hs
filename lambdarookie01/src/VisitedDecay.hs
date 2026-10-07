{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module VisitedDecay
  ( VisitedDecay
  , visitedDecayEmpty
  , visitedDecayAge
  , visitedDecayStep
  , visitedDecayVisit
  , visitedDecayScore
  )
where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import SC2.Grid.Core (Grid, gridH, gridW)
import SC2.TilePos (TilePos)

data VisitedDecay = VisitedDecay
  { vdVisitedWidth :: !Int
  , vdVisitedAges :: !(Vector (Maybe Word64))
  }
  deriving (Eq, Show)

visitedDecayEmpty :: Grid -> VisitedDecay
visitedDecayEmpty grid =
  VisitedDecay
    { vdVisitedWidth = width
    , vdVisitedAges = Vector.replicate (width * height) Nothing
    }
 where
  width = gridW grid
  height = gridH grid

visitedDecayIndex :: VisitedDecay -> TilePos -> Int
visitedDecayIndex VisitedDecay{vdVisitedWidth} (x, y) =
  y * vdVisitedWidth + x

visitedDecayAge :: VisitedDecay -> TilePos -> Maybe Word64
visitedDecayAge visits@VisitedDecay{vdVisitedAges} pos =
  vdVisitedAges Vector.! visitedDecayIndex visits pos

visitedDecayStep :: VisitedDecay -> VisitedDecay
visitedDecayStep visits@VisitedDecay{vdVisitedAges} =
  visits
    { vdVisitedAges =
        Vector.map (fmap incrementAge) vdVisitedAges
    }
 where
  incrementAge age
    | age == maxBound = maxBound
    | otherwise = age + 1

visitedDecayVisit :: TilePos -> VisitedDecay -> VisitedDecay
visitedDecayVisit pos visits@VisitedDecay{vdVisitedAges} =
  visits
    { vdVisitedAges =
        vdVisitedAges Vector.// [(visitedDecayIndex visits pos, Just 0)]
    }

visitedDecayScore :: VisitedDecay -> TilePos -> Float
visitedDecayScore visits pos =
  case visitedDecayAge visits pos of
    Nothing -> 0
    Just age -> 1 / (1 + fromIntegral age)
