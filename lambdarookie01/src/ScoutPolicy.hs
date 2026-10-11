{-# LANGUAGE DeriveGeneric #-}

module ScoutPolicy where

import GHC.Generics (Generic)

data ScoutFeatures a = ScoutFeatures
  { progress :: !a
  , information :: !a
  , threat :: !a
  , escape :: !a
  }
  deriving (Generic, Show)

-- deriving stock (Generic, Show, E)

instance (Num a) => Num (ScoutFeatures a) where
  ScoutFeatures a b c d
    + ScoutFeatures a' b' c' d' =
      ScoutFeatures (a + a') (b + b') (c + c') (d + d')

  ScoutFeatures a b c d
    - ScoutFeatures a' b' c' d' =
      ScoutFeatures (a - a') (b - b') (c - c') (d - d')

  negate (ScoutFeatures a b c d) =
    ScoutFeatures
      (negate a)
      (negate b)
      (negate c)
      (negate d)

  fromInteger x =
    let a = fromInteger x
     in ScoutFeatures a a a a

  signum (ScoutFeatures a b c d) =
    ScoutFeatures
      (signum a)
      (signum b)
      (signum c)
      (signum d)

dot :: (Num a) => ScoutFeatures a -> ScoutFeatures a -> a
dot
  (ScoutFeatures a b c d)
  (ScoutFeatures a' b' c' d') =
    a * a'
      + b * b'
      + c * c'
      + d * d'

scale :: (Num a) => a -> ScoutFeatures a -> ScoutFeatures a
scale k (ScoutFeatures a b c d) =
  ScoutFeatures
    (k * a)
    (k * b)
    (k * c)
    (k * d)

type ScoutWeights = ScoutFeatures Double
