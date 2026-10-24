module ScoringUtils where

saturatingScore :: Float -> Float -> Float
saturatingScore value scale =
  let nonNegativeValue = max 0 value
   in nonNegativeValue / (nonNegativeValue + scale)

clamp01 :: Float -> Float
clamp01 = max 0 . min 1
