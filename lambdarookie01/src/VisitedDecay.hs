{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module VisitedDecay
  ( VisitedDecay
  , visitedDecayEmpty
  , visitedDecayAge
  , visitedDecayVisit
  , calcVisitedDecayScore
  )
where

import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word32)
import Debug.Trace (trace)
import Lens.Micro ((<&>), (^.))
import SC2.Grid.Core (Grid, gridH, gridW)
import SC2.TilePos (TilePos)
import StepMonad (HasObs, StepMonad, agentObs)

data VisitedDecay = VisitedDecay
  { vdVisitedWidth :: !Int
  , vdVisitedAt :: !(Vector (Maybe Word32))
  }
  deriving (Eq, Show)

visitedDecayEmpty :: Grid -> VisitedDecay
visitedDecayEmpty grid =
  VisitedDecay
    { vdVisitedWidth = width
    , vdVisitedAt = Vector.replicate (width * height) Nothing
    }
 where
  width = gridW grid
  height = gridH grid

visitedDecayIndex :: VisitedDecay -> TilePos -> Int
visitedDecayIndex VisitedDecay{vdVisitedWidth} (x, y) =
  y * vdVisitedWidth + x

visitedDecayAge :: Word32 -> VisitedDecay -> TilePos -> Word32
visitedDecayAge frame visits@VisitedDecay{vdVisitedAt} pos =
  -- frame <- agentObs <&> (^. #gameLoop)
  let visitedAt = fromMaybe frame (vdVisitedAt Vector.! visitedDecayIndex visits pos)
   in frame - visitedAt

visitedDecayVisit :: Word32 -> TilePos -> VisitedDecay -> VisitedDecay
visitedDecayVisit frame pos visits@VisitedDecay{vdVisitedAt} =
  trace
    ("visiting " ++ show pos ++ " at " ++ show frame)
    visits
      { vdVisitedAt =
          vdVisitedAt Vector.// [(visitedDecayIndex visits pos, Just frame)]
      }

calcVisitedDecayScore ::
  (HasObs d) =>
  TilePos ->
  VisitedDecay ->
  StepMonad d Float
calcVisitedDecayScore candidate visited = do
  frame <- agentObs <&> (^. #gameLoop)
  pure $ visitedDecayScore frame visited candidate

visitedDecayScore :: Word32 -> VisitedDecay -> TilePos -> Float
visitedDecayScore frame visits pos =
  let age = visitedDecayAge frame visits pos
   in frameDeltaScore age 1000 -- TODO: pick threshold

frameDeltaScore :: Word32 -> Word32 -> Float
frameDeltaScore d threshold
  | d == 0 = 1
  | otherwise =
      max 0 ((fromIntegral d - fromIntegral threshold * 0.8) / (fromIntegral threshold * 0.2))
