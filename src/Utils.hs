
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils ((-), distSquared) where

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A

import Data.ProtoLens (defMessage)
import Lens.Micro((&), (.~), (^.))

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
   a - b = defMessage & C.x .~ ((a ^. x) - (b ^. x)) & C.y .~ ((a ^. y) - (b ^. y))

createPoint2D x y = defMessage & C.x .~ x & C.y .~ y

dot a b = (a ^. x) * (b ^. x) + (a ^. y) * (b ^. y)

distSquared a b = dot diff diff where
  diff = a - b
