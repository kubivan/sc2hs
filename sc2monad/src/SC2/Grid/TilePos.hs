{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SC2.Grid.TilePos where

import SC2.Geometry (tileX3D, tileY3D)
import SC2.Proto.Data (Point)

type TilePos = (Int, Int)

tilePos :: Point -> TilePos
tilePos p = (tileX3D p, tileY3D p)

instance Num TilePos where
    (+) (ax, ay) (bx, by) = (ax + bx, ay + by)
    (-) (ax, ay) (bx, by) = (ax - bx, ay - by)
    (*) = undefined
    abs (x, y) = (abs x, abs y)
    signum (x, y) = (signum x, signum y)
    fromInteger n = (fromInteger n, fromInteger n)
    negate (x, y) = (negate x, negate y)
