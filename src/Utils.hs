
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Utils ((-), distSquared, dot, enemyBaseLocation, unitsSelf, Pointable(..)) where

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A

import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Raw_Fields as R

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

instance Num C.Point2D where
   a - b = defMessage & C.x .~ (a ^. C.x) - (b ^. C.x) & C.y .~ (a ^. C.y) - (b ^. C.y)

createPoint2D x y = defMessage & C.x .~ x & C.y .~ y

dot a b = a ^. C.x * b ^. C.x + a ^. C.y * b ^. C.y

distSquared a b = dot diff diff where
  diff = a - b

toPoint2D p = defMessage & C.x .~ (p ^. #x) & C.x .~ (p ^. #x)

class Pointable a where
  to2D :: a -> C.Point2D
  x :: a -> Float
  y :: a -> Float

instance Pointable C.Point2D where
  x p = p ^. C.x
  y p = p ^. C.y

  to2D p = p

instance Pointable C.Point where
  x p = p ^. C.x
  y p = p ^. C.y

  to2D p = createPoint2D (Utils.x p) (Utils.y p)

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

debug = flip trace

protossNexus :: GHC.Word.Word32
protossNexus = 59

unitsSelf obs = filter (\u -> u ^. alliance == A.Self) (obs ^. (A.rawData . R.units))

enemyBaseLocation :: A.ResponseGameInfo -> A.Observation -> C.Point2D
enemyBaseLocation gi obs = head $ filter notCloseToNexus enemyBases where
  nexus = head $ filter (\u -> (u ^. R.unitType) == protossNexus) (unitsSelf obs) -- `Utils.debug` ("unitself " ++ (show unitsSelf))
  notCloseToNexus p = distSquared p (to2D (nexus ^. R.pos) ) > 1
  enemyBases = gi ^. (A.startRaw . R.startLocations)
