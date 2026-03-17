module Target (Target (..)) where

import SC2.Grid.TilePos (TilePos)
import SC2.Spatial (Spatial (..))
import Units (Unit)

data Target
    = TargetPos TilePos
    | TargetUnit Unit
    deriving (Eq, Show)

instance Spatial Target where
    toTilePos (TargetPos pos) = toTilePos pos
    toTilePos (TargetUnit unit) = toTilePos unit
    toPoint2D (TargetPos pos) = toPoint2D pos
    toPoint2D (TargetUnit unit) = toPoint2D unit
