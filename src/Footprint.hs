module Footprint (getFootprint, Footprint(..), createFootprint) where

import UnitTypeId
import Data.List (elemIndices)
import Control.Monad (join)

data Footprint = Footprint
  { footprint :: [(Int, Int)]
  , mark :: Char
  } deriving(Show, Eq)

findMark :: [Char] -> Char
findMark chars = head [c | c <- chars, c /= ' ' && c /= 'c']

createFootprint :: String -> Footprint
createFootprint str = Footprint footprint (findMark str) where
    rows = lines str
    footprint = [ translatePoint (x, y) (ox, oy) | y <- [0..h-1], x <- [0..w-1] , (join rows !! (x + y*h)) /= ' ']
    ox = originIndex - w * oy 
    oy = originIndex `div` w
    w = length.head $ rows
    h = length rows
    originIndex = head $ elemIndices 'c' (join rows)
    translatePoint (x, y) (ox, oy) = (x - ox, y - oy)

getFootprint :: UnitTypeId -> Footprint
getFootprint id = case id of 
  ProtossNexus -> createFootprint $ unlines [ "#####"
                                            , "#   #"
                                            , "# c #"
                                            , "#   #"
                                            , "#####"]
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
  NeutralMineralfield -> createFootprint "#c"
  NeutralMineralfield750 -> createFootprint "#c"
  ProtossProbe -> createFootprint "#c"

  _ -> createFootprint $ unlines [ "###"
                                 , "#c#"
                                 , "###"]

  