
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils (
  (-)
  , distSquared
  , distSquaredTile
  , distManhattan
  , dot
  , Pointable(..)
  , tileX
  , tileY
  , tilePos
  , fromTuple
  , to2D
  , dbg
  , triPartition
  , TilePos) where

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api as A
    ( ResponseGameInfo, Observation )
import Proto.S2clientprotocol.Sc2api_Fields as A

import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Raw_Fields as R
    ( alliance, pos, startLocations, unitType, units )

import Data.ProtoLens (defMessage)
import Lens.Micro((&), (.~), (^.))
import qualified GHC.Word
import qualified Proto.S2clientprotocol.Raw as A

import Debug.Trace

-- Custom operator for addition
--infixl 6 -.
-- (-) :: C.Point2D -> C.Point2D -> C.Point2D
-- a - b = defMessage & C.x .~ rx & C.y .~ ry :: C.Point2D where
--     rx = (a ^. x) Prelude.- (b ^. x)
--     ry = (a ^. y) Prelude.- (b ^. y)
--

--distSquared a b =
--instance (Num a,Num b) => Num (C.Point2D) where
--   a - b = defMessage & C.x .~ ((a ^. x) Prelude.- (b ^. x)) & C.y .~ ((a ^. y) Prelude.- (b ^. y)) where

--   Pair (a,b) + Pair (c,d) = Pair (a+c,b+d)
--   Pair (a,b) * Pair (c,d) = Pair (a*c,b*d)
--   abs    (Pair (a,b)) = Pair (abs a,    abs b)
--   signum (Pair (a,b)) = Pair (signum a, signum b)
--   fromInteger i = Pair (fromInteger i, fromInteger i)

class Pointable a where
  make2D :: Float -> Float -> a
  x :: a -> Float
  y :: a -> Float

instance Num C.Point2D where
  a - b = make2D (x1 `subtract` x2) (y1 `subtract` y2) where
    x1 = (Utils.x a) :: Float
    x2 = (Utils.x b) :: Float
    y1 = (Utils.y a) :: Float
    y2 = (Utils.y b) :: Float

  (+) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

dot :: Pointable p => p -> p -> Float
dot a b = Utils.x a * Utils.x b + Utils.y a* Utils.y b

distSquared :: (Pointable p1, Pointable p2) => p1 -> p2 -> Float
distSquared a b = dot diff diff where
  diff = to2D a - to2D b

distManhattan :: TilePos -> TilePos -> Int
distManhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

--TODO: there shoud be the way to clean this mess with Pointables
--to have one distSquared and so on
distSquaredTile :: TilePos -> TilePos -> Float
distSquaredTile (ax, ay) (bx, by) = dot diff diff where
  diff :: C.Point2D
  diff = defMessage & C.x .~ fromIntegral (ax - bx) & C.y .~ fromIntegral (ay - by)

toPoint2D p = defMessage & C.x .~ (p ^. #x) & C.y .~ (p ^. #y)

fromTuple :: (Integral a) => (a, a) -> Point2D
fromTuple (px, py) = defMessage & C.x .~ fromIntegral px & C.y .~ fromIntegral py

tileX :: Pointable a => a -> Int
tileX = floor . Utils.x

tileY :: Pointable a => a -> Int
tileY = floor . Utils.y

type TilePos = (Int, Int)

tilePos :: Pointable a => a -> TilePos
tilePos p = (tileX p, tileY p)

--tileY :: Pointable a => a -> Int

instance Pointable C.Point2D where
  x p = p ^. C.x
  y p = p ^. C.y

  make2D xp yp = defMessage & C.x .~ xp & C.y .~ yp

instance Pointable C.Point where
  x p = p ^. C.x
  y p = p ^. C.y

to2D :: Pointable a => a -> Point2D
to2D p = make2D (Utils.x p) (Utils.y p)
-- type Point2DI = (Int, Int)
-- instance Pointable Point2DI where
--   x (a, _) = fromIntegral a
--   y (_, b) = fromIntegral b
--   to2D p = createPoint2D (Utils.x p) (Utils.y p)

-- Point2D
-- enemy_base_location(const Observation& obs)
-- {
--     static Point2D res = [&obs]() {
--         using namespace std::views;
--         const auto nexus = to_vector<Unit>(obs.unitsSelf() | filter(type(UNIT_TYPEID::PROTOSS_NEXUS))).front();
--
--         for (auto enemy_pos : obs.gameInfo().start_locations)
--         {
--             if (dist_squared(enemy_pos, nexus.pos) > 1.)
--             {
--                 continue;
--             }
--             return enemy_pos;
--         }
--         assert(false);
--         return Point2D{ -1, -1 };
--     }();
--
--     return res;
-- }

dbg = flip trace

triPartition :: (a -> Ordering) -> [a] -> ([a], [a], [a])
triPartition cmp = foldr partition ([], [], [])
  where
    partition x (less, eq, greater) =
      case cmp x of
        LT -> (x : less, eq, greater)
        EQ -> (less, x : eq, greater)
        GT -> (less, eq, x : greater)
