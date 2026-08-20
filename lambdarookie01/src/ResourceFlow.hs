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
  { mineralRate :: Double
  , gasRate :: Double
  }
  deriving (Show, Eq)

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
          let t = lastSample.incomeRes - firstSample.incomeRes + spent
           in ResourceRate
                { mineralRate = (fromIntegral t.mineralCost) / fromIntegral frameDelta
                , gasRate = (fromIntegral t.gasCost) / fromIntegral frameDelta
                }
     where
      frameDelta =
        lastSample.frame - firstSample.frame

      -- Spending associated with frames inside the interval.
      interval =
        Seq.drop 1 xs
      spent = sum $ (.spentRes) <$> interval
    _ ->
      ResourceRate 0 0

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
