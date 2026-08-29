{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SC2.TilePos(TilePos(..)) where

type TilePos = (Int, Int)

instance Num TilePos where
  (+) (ax, ay) (bx, by) = (ax + bx, ay + by)
  (-) (ax, ay) (bx, by) = (ax - bx, ay - by)
  (*) = undefined
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger n = (fromInteger n, fromInteger n)
  negate (x, y) = (negate x, negate y)
