module Footprint (getFootprint, Footprint(..), createFootprint) where

import UnitTypeId
import Data.List (elemIndices)
import Control.Monad (join)

newtype Footprint = Footprint { pixels :: [(Int, Int, Char)] } deriving(Show, Eq)

createFootprint :: String -> Footprint
createFootprint str = Footprint pixels where
    rows = lines str
    pixels = [ (x - ox, y - oy, pixel) | y <- [0..h-1], x <- [0..w-1] , let pixel = rows !! y !! x, pixel /= ' ']
    ox = originIndex - w * oy 
    oy = originIndex `div` w
    w = length.head $ rows
    h = length rows
    originIndex = head $ elemIndices 'c' (join rows)

getFootprint :: UnitTypeId -> Footprint
getFootprint id = case id of 
  ProtossNexus -> createFootprint $ unlines [ "nnnnn"
                                            , "nnnnn"
                                            , "nncnn"
                                            , "nnnnn"
                                            , "nnnnn"]
  ProtossForge -> createFootprint $ unlines [ "###"
                                            , "#c#"
                                            , "###"]
  ProtossAssimilator -> createFootprint $ unlines [ "###"
                                                  , "#c#"
                                                  , "###"]
  NeutralRichvespenegeyser -> createFootprint $ unlines [ "###"
                                                        , "#c#"
                                                        , "###"]
  ProtossPylon -> createFootprint $ unlines ["##"
                                            ,"#c"]
  ProtossPhotoncannon -> createFootprint $ unlines [ "...."
                                                   , ".##."
                                                   , ".#c."
                                                   , "...."
                                                   ]
  NeutralMineralfield -> createFootprint "#c"
  NeutralMineralfield750 -> createFootprint "#c"
  --ProtossProbe -> createFootprint "#c"

  _ -> createFootprint $ unlines [ "###"
                                 , "#c#"
                                 , "###"]

  
