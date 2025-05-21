module SC2.Geometry where

import Proto.S2clientprotocol.Common ( Point, Point2D )
import Proto.S2clientprotocol.Common_Fields ( x, y, z )

import Data.ProtoLens (defMessage)
import Lens.Micro((&), (.~), (^.))
import Lens.Micro.Extras ( view )

toPoint2D :: Pointable a => a -> Point2D
toPoint2D p = defMessage & x .~ getX p & y .~ getY p

toPoint3D :: Pointable a => a -> Point
toPoint3D p = defMessage & x .~ getX p & y .~ getY p & z .~ 0

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
  (+) a b = defMessage & x .~ (a ^. x + b ^. x) & y .~ (a ^. y + b ^. y) & z .~ (a ^. z + b ^. z)
  (-) a b = defMessage & x .~ (a ^. x - b ^. x) & y .~ (a ^. y - b ^. y) & z .~ (a ^. z - b ^. z)
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


instance Pointable Point2D where
  getX = view x
  getY = view y

  makePoint px py = defMessage & x .~ px & y .~ py

instance Pointable Point where
  getX = view x
  getY = view y
  makePoint px py = defMessage & x .~ px & y .~ py & z .~ 0


vecNormalize :: Point2D -> Point2D
vecNormalize v =
    let (vx, vy) = (v ^. x , v ^. y)
        vlen = sqrt $ vx*vx + vy*vy
    in defMessage & x .~ (vx / vlen) & y .~ (vy / vlen)

vecScale :: Float -> Point2D -> Point2D
vecScale s v =
    let (vx, vy) = (v ^. x , v ^. y)
    in defMessage & x .~ (vx * s) & y .~ (vy * s)