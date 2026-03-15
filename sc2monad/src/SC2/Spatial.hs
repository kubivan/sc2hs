{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SC2.Spatial
  ( Spatial (..)
  , distSquared
  , distManhattan
  , distSquared2D
  , dist2D
  ) where

import Lens.Micro ((^.))
import SC2.Geometry qualified as Geometry
import SC2.Grid.TilePos (TilePos)
import SC2.Proto.Data (Point, Point2D, Unit)

class Spatial a where
  toTilePos :: a -> TilePos
  toPoint2D :: a -> Point2D

instance Spatial TilePos where
  toTilePos = id
  toPoint2D = Geometry.fromTuple

instance Spatial Point2D where
  toTilePos p = (Geometry.tileX p, Geometry.tileY p)
  toPoint2D = id

instance Spatial Point where
  toTilePos p = (Geometry.tileX3D p, Geometry.tileY3D p)
  toPoint2D = Geometry.toPoint2D

instance Spatial Unit where
  toTilePos unit = toTilePos (unit ^. #pos)
  toPoint2D unit = toPoint2D (unit ^. #pos)

distSquared :: (Spatial a, Spatial b) => a -> b -> Int
distSquared a b = dx * dx + dy * dy
  where
    (ax, ay) = toTilePos a
    (bx, by) = toTilePos b
    dx = ax - bx
    dy = ay - by

distManhattan :: (Spatial a, Spatial b) => a -> b -> Int
distManhattan a b = abs (ax - bx) + abs (ay - by)
  where
    (ax, ay) = toTilePos a
    (bx, by) = toTilePos b

distSquared2D :: (Spatial a, Spatial b) => a -> b -> Float
distSquared2D a b = Geometry.distSquared (toPoint2D a) (toPoint2D b)

dist2D :: (Spatial a, Spatial b) => a -> b -> Float
dist2D a b = sqrt (distSquared2D a b)
