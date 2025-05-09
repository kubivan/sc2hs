{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SC2.Grid.TilePos where

import SC2.Geometry

type TilePos = (Int, Int)

tilePos :: (Pointable a) => a -> TilePos
tilePos p = (tileX p, tileY p)

instance Num TilePos where
    (+) = pointPlus -- Use the + operator from Pointable
    (-) = pointMinus -- Use the - operator from Pointable
    (*) = undefined -- Multiplication may not make sense, so leave it undefined or customize it
    abs p = makePoint (abs $ getX p) (abs $ getY p)
    signum p = makePoint (signum $ getX p) (signum $ getY p)
    fromInteger n = makePoint (fromInteger n) (fromInteger n)
    negate p = makePoint (negate $ getX p) (negate $ getY p)

instance Pointable TilePos where
    getX (x, _) = fromIntegral x
    getY (_, y) = fromIntegral y
    makePoint x y = (floor x, floor y)
