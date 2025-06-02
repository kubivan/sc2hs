module Footprint (getFootprint, Footprint(..), createFootprint, footprintRect) where

import SC2.Ids.UnitTypeId
import Data.List (elemIndices)
import Control.Monad (join)
import SC2.Grid.TilePos ( TilePos )

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

footprintRect :: (TilePos, TilePos) -> (Footprint, TilePos)
footprintRect (origin@(tlX, tlY), (brX, brY)) = (Footprint {pixels = topH ++ bottomH ++ leftV ++ rightV}, origin)
  where
    topH = [(x, 0, '#') | x <- [0..(brX - tlX)]]
    bottomH = [(x, brY - tlY, '#') | x <- [0..(brX - tlX)]]
    leftV = [(0, y, '#') | y <- [1..(brY - tlY)]]
    rightV = [(brX - tlX, y, '#') | y <- [1..(brY - tlY)]]

getFootprint :: UnitTypeId -> Footprint
getFootprint id = case id of
  ProtossNexus -> createFootprint
    $ unlines [ "nnnnn"
              , "nnnnn"
              , "nncnn"
              , "nnnnn"
              , "nnnnn"]
  ProtossForge -> createFootprint
    $ unlines [ "###"
              , "#c#"
              , "###"]
  ProtossAssimilator -> createFootprint
    $ unlines [ "###"
              , "#c#"
              , "###"]
  NeutralVespeneGeyser -> createFootprint
    $ unlines [ " ....... "
              , "........."
              , "........."
              , "...###..."
              , "...#c#..."
              , "...###..."
              , "........."
              , "........."
              , " ....... "
              ]
  NeutralRichVespeneGeyser -> createFootprint
    $ unlines [ " ....... "
              , "........."
              , "........."
              , "...###..."
              , "...#c#..."
              , "...###..."
              , "........."
              , "........."
              , " ....... "
              ]

  ProtossPylon -> createFootprint
    $ unlines ["##"
              ,"#c"]

  ProtossPhotonCannon -> createFootprint
    $ unlines [ "...."
              , ".##."
              , ".#c."
              , "...."
              ]
  NeutralMineralField -> createFootprint
    $ unlines [ " ...... "
              , "........"
              , "........"
              , "...#c..."
              , "........"
              , "........"
              , " ...... "
              ]
  NeutralMineralField750 -> createFootprint "#c"

  _ -> createFootprint
    $ unlines [ "###"
              , "#c#"
              , "###"]


