{-# LANGUAGE OverloadedRecordDot #-}

module ResourceFlow where

import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Observation (Cost (..))

resourceRateWindow :: Int
resourceRateWindow = 224

data ResourceRateState = ResourceRateState
  { samples :: Seq ResourceSample
  , rate :: ResourceRate
  }
  deriving (Show, Eq)

data ResourceRate = ResourceRate
  { incomeRate :: CostRate
  , consumptionRate :: CostRate
  }
  deriving (Show, Eq)

data CostRate = CostRate
  { mineralRate :: Double
  , gasRate :: Double
  }
  deriving (Show, Eq, Ord)

instance Num CostRate where
  a + b = CostRate (mineralRate a + mineralRate b) (gasRate a + gasRate b)
  a - b = CostRate (mineralRate a - mineralRate b) (gasRate a - gasRate b)
  a * b = CostRate (mineralRate a * mineralRate b) (gasRate a * gasRate b)
  negate (CostRate mc gc) = CostRate (-mc) (-gc)
  abs (CostRate mc gc) = CostRate (abs mc) (abs gc)
  signum (CostRate mc gc) = CostRate (signum mc) (signum gc)
  fromInteger n = CostRate (fromInteger n) (fromInteger n)

data ResourceSample = ResourceSample
  { frame :: Int
  , incomeRes :: Cost
  , spentRes :: Cost
  }
  deriving (Show, Eq)

calculateResourceRate :: Seq ResourceSample -> ResourceRate
calculateResourceRate xs =
  case (Seq.lookup 0 xs, Seq.lookup (Seq.length xs - 1) xs) of
    (Just firstSample, Just lastSample)
      | frameDelta > 0 ->
          ResourceRate
            { incomeRate =
                CostRate
                  { mineralRate =
                      fromIntegral
                        (resourceDelta.mineralCost + spent.mineralCost)
                        / fromIntegral frameDelta
                  , gasRate =
                      fromIntegral
                        (resourceDelta.gasCost + spent.gasCost)
                        / fromIntegral frameDelta
                  }
            , consumptionRate =
                CostRate
                  { mineralRate =
                      fromIntegral spent.mineralCost
                        / fromIntegral frameDelta
                  , gasRate =
                      fromIntegral spent.gasCost
                        / fromIntegral frameDelta
                  }
            }
     where
      frameDelta =
        lastSample.frame - firstSample.frame

      resourceDelta =
        lastSample.incomeRes - firstSample.incomeRes

      spent =
        sum $ (.spentRes) <$> Seq.drop 1 xs
    _ ->
      ResourceRate
        { incomeRate = CostRate 0 0
        , consumptionRate = CostRate 0 0
        }

updateResourceRate ::
  -- | current frame
  Int ->
  Cost -> -- current minerals

  -- | resources spent this frame
  Cost ->
  -- | window in frames
  Int ->
  ResourceRateState ->
  ResourceRateState
updateResourceRate currentFrame currentRes frameSpent window state =
  let newSample =
        ResourceSample
          { frame = currentFrame
          , incomeRes = currentRes
          , spentRes = frameSpent
          }

      newSamples =
        trimSamples window $
          samples state |> newSample

      newRate =
        calculateResourceRate newSamples
   in state
        { samples = newSamples
        , rate = newRate
        }

trimSamples :: Int -> Seq ResourceSample -> Seq ResourceSample
trimSamples window xs =
  case Seq.lookup (Seq.length xs - 1) xs of
    Nothing ->
      xs
    Just newest ->
      Seq.dropWhileL
        (\sample -> newest.frame - sample.frame > window)
        xs

-- unitBuildRate :: Unit -> CostRate
