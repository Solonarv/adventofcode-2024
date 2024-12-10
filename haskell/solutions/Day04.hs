{-# LANGUAGE OverloadedLists #-}
module Day04 where

import AOC.Solution
import ParsingPrelude
import Grid2D
import Util

import Data.IntMap.Strict qualified as IntMap
import Data.List
import Linear.V2

solution :: Solution (Grid2D Char) Int Int
solution = Solution
  { decodeInput = fromLines <$> many nonspace `sepBy` eol
  , solveA = defSolver
    { solve = Just . length . findPattern "XMAS"
    }
  , solveB = defSolver
    { solve = Just . length . findCrossPattern "MAS"
    }
  , tests =
    [ unlines
      [ "MMMSXXMASM"
      , "MSAMXMSMSA"
      , "AMXSXMAAMM"
      , "MSAMASMSMX"
      , "XMASAMXAMM"
      , "XXAMMXXAMA"
      , "SMSMSASXSS"
      , "SAXAMASAAA"
      , "MAMMMXMMMM"
      , "MXMXAXMASX"
      ] :=> [(PartA, "18"), (PartB, "9")]
    ]
  }

findPattern :: Eq a => [a] -> Grid2D a -> [[V2 Int]]
findPattern pat g = map (map fst) . filter ((pat `isPrefixOf`) . map snd) $ concat
  [ allRaysWithCoords dx dy g
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , dx /= 0 || dy /= 0
  ]

findCrossPattern :: Ord a => [a] -> Grid2D a -> [V2 Int]
findCrossPattern pat g = crosses
  where
    diagRays = concat
        [ allRaysWithCoords dx dy g
        | dx <- [-1, 1]
        , dy <- [-1, 1]
        ]
    validRays = filter ((pat `isPrefixOf`) . map snd) diagRays
    fulcrums = map fst $ mapMaybe (safeIndex 1) validRays
    crosses = IntMap.findWithDefault [] 2 $ invert (toFreqMap fulcrums)