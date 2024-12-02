module Day02 where

import AOC.Solution

import ParsingPrelude
import Util

solution :: Solution [[Int]] Int Int
solution = Solution
  { decodeInput = (decimal `sepBy` noeol) `sepBy` eol
  , solveA = defSolver
    { solve = Just . countHits safeReport . map diffs
    }
  , solveB = defSolver
    { solve = Just . countHits safeDampened
    }
  , tests =
    [ unlines
      [ "7 6 4 2 1"
      , "1 2 7 8 9"
      , "9 7 6 2 1"
      , "1 3 2 4 5"
      , "8 6 4 4 1"
      , "1 3 6 7 9"
      ] :=> [(PartA, "2"), (PartB, "4")]
    ]
  }



safeReport :: [Int] -> Bool
safeReport = all increasing <||> all decreasing
  where
    decreasing d = d >= 1 && d <= 3
    increasing d = decreasing (-d)

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) xs (drop 1 xs)

safeDampened :: [Int] -> Bool
safeDampened = any safeReport . map diffs .  mapMaybe mergeSplit . splits
  where
    mergeSplit (as, _:bs) = Just $ as ++ bs
    mergeSplit _ = Nothing