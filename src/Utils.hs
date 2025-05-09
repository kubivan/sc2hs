
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}

-- TODO: move Pointable to the separate module
module Utils (
  (-)
  , distSquared
  , distManhattan
  , dot
  , Pointable(..)
  , tileX
  , tileY
  , tilePos
  , fromTuple
  , toPoint2D
  , dbg
  , triPartition
  , TilePos) where

import Proto.S2clientprotocol.Common ( Point, Point2D )
import Proto.S2clientprotocol.Common_Fields ( x, y, z )

import Data.ProtoLens (defMessage)
import Lens.Micro((&), (.~), (^.))
import Lens.Micro.Extras ( view )

import Debug.Trace

toPoint2D :: Pointable a => a -> Point2D
toPoint2D p = defMessage & x .~ (0.5 + getX p) & y .~ (0.5 + getY p)

class Pointable p where
  getX :: p -> Float
  getY :: p -> Float
  makePoint :: Float -> Float -> p

  pointPlus :: p -> p -> p
  a `pointPlus` b = makePoint (getX a + getX b) (getY a + getY b)

  pointMinus :: p -> p -> p
  a `pointMinus` b = makePoint (getX a - getX b) (getY a - getY b)

instance Num Point2D where
  (+) = pointPlus  -- Use the + operator from Pointable
  (-) = pointMinus  -- Use the - operator from Pointable
  (*) = undefined  -- Multiplication may not make sense, so leave it undefined or customize it
  abs p = makePoint (abs $ getX p) (abs $ getY p)
  signum p = makePoint (signum $ getX p) (signum $ getY p)
  fromInteger n = makePoint (fromInteger n) (fromInteger n)
  negate p = makePoint (negate $ getX p) (negate $ getY p)

instance Num Point where
  (+) = pointPlus  -- Use the + operator from Pointable
  (-) = pointMinus  -- Use the - operator from Pointable
  (*) = undefined  -- Multiplication may not make sense, so leave it undefined or customize it
  abs p = makePoint (abs $ getX p) (abs $ getY p)
  signum p = makePoint (signum $ getX p) (signum $ getY p)
  fromInteger n = makePoint (fromInteger n) (fromInteger n)
  negate p = makePoint (negate $ getX p) (negate $ getY p)

instance Num TilePos where
  (+) = pointPlus  -- Use the + operator from Pointable
  (-) = pointMinus  -- Use the - operator from Pointable
  (*) = undefined  -- Multiplication may not make sense, so leave it undefined or customize it
  abs p = makePoint (abs $ getX p) (abs $ getY p)
  signum p = makePoint (signum $ getX p) (signum $ getY p)
  fromInteger n = makePoint (fromInteger n) (fromInteger n)
  negate p = makePoint (negate $ getX p) (negate $ getY p)

dot :: Pointable p => p -> p -> Float
dot a b = getX a * getX b + getY a * getY b

distSquared :: (Pointable p1, Pointable p2) => p1 -> p2 -> Float
--distSquared :: (Pointable p) => p -> p -> Float
distSquared a b = dot diff diff where
  diff = toPoint2D a - toPoint2D b

distManhattan :: (Pointable p1, Pointable p2) => p1 -> p2 -> Int
distManhattan p1 p2 = abs (tileX p1 - tileX p2) + abs (tileY p1 - tileY p2)

fromTuple :: (Integral a) => (a, a) -> Point2D
fromTuple (px, py) = defMessage & x .~ fromIntegral px & y .~ fromIntegral py

tileX :: Pointable a => a -> Int
tileX = floor . getX

tileY :: Pointable a => a -> Int
tileY = floor . getY

type TilePos = (Int, Int)

tilePos :: Pointable a => a -> TilePos
tilePos p = (tileX p, tileY p)

instance Pointable Point2D where
  getX = view x
  getY = view y

  makePoint px py = defMessage & x .~ px & y .~ py

instance Pointable Point where
  getX = view x
  getY = view y
  makePoint px py = defMessage & x .~ px & y .~ py & z .~ 0

instance Pointable TilePos where
  getX (x, _) = fromIntegral x
  getY (_, y) = fromIntegral y
  makePoint x y = (floor x, floor y)

dbg = flip trace

triPartition :: (a -> Ordering) -> [a] -> ([a], [a], [a])
triPartition cmp = foldr partition ([], [], [])
  where
    partition x (less, eq, greater) =
      case cmp x of
        LT -> (x : less, eq, greater)
        EQ -> (less, x : eq, greater)
        GT -> (less, eq, x : greater)