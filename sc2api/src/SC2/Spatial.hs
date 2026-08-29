{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SC2.Spatial
  ( Spatial (..)
  , distSquaredI
  , distSquaredF
  , distManhattan
  , tilePos
  , tileX3D
  , tileY3D
  , toPoint3D
  ) where

import Lens.Micro ((^.), (.~), (&))
import SC2.TilePos(TilePos)
import SC2.Geometry
import SC2.Proto.Data (Point, Point2D, Unit)
import Data.ProtoLens (defMessage)
import Lens.Micro.Extras (view)

tilePos :: (Spatial a) => a -> TilePos
tilePos = toTilePos

class Spatial a where
  toTilePos :: a -> TilePos
  toPoint2D :: a -> Point2D

instance Spatial TilePos where
  toTilePos = id
  toPoint2D (px, py) = defMessage & #x .~ fromIntegral px & #y .~ fromIntegral py

instance Spatial Point2D where
  toTilePos p = (tileX p, tileY p)
  toPoint2D = id

instance Spatial Point where
  toTilePos p = (tileX3D p, tileY3D p)
  toPoint2D p = defMessage & #x .~ (p ^. #x) & #y .~ (p ^. #y)


instance Spatial Unit where
  toTilePos unit = toTilePos (unit ^. #pos)
  toPoint2D unit = toPoint2D (unit ^. #pos)

distSquaredI :: (Spatial a, Spatial b) => a -> b -> Int
distSquaredI a b = dx * dx + dy * dy
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

distSquaredF :: (Spatial a, Spatial b) => a -> b -> Float
distSquaredF a b = distSquaredP2D (toPoint2D a) (toPoint2D b)

toPoint3D :: Point2D -> Point
toPoint3D p = defMessage & #x .~ (p ^. #x) & #y .~ (p ^. #y) & #z .~ 0

tileX :: Point2D -> Int
tileX = floor . view #x

tileY :: Point2D -> Int
tileY = floor . view #y

tileX3D :: Point -> Int
tileX3D = floor . view #x

tileY3D :: Point -> Int
tileY3D = floor . view #y

dot :: Point2D -> Point2D -> Float
dot a b = (a ^. #x) * (b ^. #x) + (a ^. #y) * (b ^. #y)

distSquaredP2D :: Point2D -> Point2D -> Float
distSquaredP2D a b = dot diff diff
  where
   diff = a - b
