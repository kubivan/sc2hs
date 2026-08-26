module SC2.Geometry(vecNormalize, vecScale) where

import Proto.S2clientprotocol.Common (Point, Point2D)
import Proto.S2clientprotocol.Common_Fields (x, y, z)

import Data.ProtoLens (defMessage)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Extras (view)

instance Num Point2D where
  (+) a b = defMessage & x .~ (a ^. x + b ^. x) & y .~ (a ^. y + b ^. y)
  (-) a b = defMessage & x .~ (a ^. x - b ^. x) & y .~ (a ^. y - b ^. y)
  (*) = undefined
  abs p = defMessage & x .~ abs (p ^. x) & y .~ abs (p ^. y)
  signum p = defMessage & x .~ signum (p ^. x) & y .~ signum (p ^. y)
  fromInteger n = defMessage & x .~ fromInteger n & y .~ fromInteger n
  negate p = defMessage & x .~ negate (p ^. x) & y .~ negate (p ^. y)

instance Num Point where
  (+) a b = defMessage & x .~ (a ^. x + b ^. x) & y .~ (a ^. y + b ^. y) & z .~ (a ^. z + b ^. z)
  (-) a b = defMessage & x .~ (a ^. x - b ^. x) & y .~ (a ^. y - b ^. y) & z .~ (a ^. z - b ^. z)
  (*) = undefined
  abs p = defMessage & x .~ abs (p ^. x) & y .~ abs (p ^. y) & z .~ abs (p ^. z)
  signum p = defMessage & x .~ signum (p ^. x) & y .~ signum (p ^. y) & z .~ signum (p ^. z)
  fromInteger n = defMessage & x .~ fromInteger n & y .~ fromInteger n & z .~ fromInteger n
  negate p = defMessage & x .~ negate (p ^. x) & y .~ negate (p ^. y) & z .~ negate (p ^. z)

dot :: Point2D -> Point2D -> Float
dot a b = (a ^. x) * (b ^. x) + (a ^. y) * (b ^. y)

fromTuple :: (Integral a) => (a, a) -> Point2D
fromTuple (px, py) = defMessage & x .~ fromIntegral px & y .~ fromIntegral py

vecNormalize :: Point2D -> Point2D
vecNormalize v =
  let (vx, vy) = (v ^. x, v ^. y)
      vlen = sqrt $ vx * vx + vy * vy
   in defMessage & x .~ (vx / vlen) & y .~ (vy / vlen)

vecScale :: Float -> Point2D -> Point2D
vecScale s v =
  let (vx, vy) = (v ^. x, v ^. y)
   in defMessage & x .~ (vx * s) & y .~ (vy * s)
