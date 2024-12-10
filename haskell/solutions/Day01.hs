module Day01 where

import AOC.Solution

import ParsingPrelude
import Util

import Control.Lens
import Data.Map (Map)
import Data.Map.Strict qualified as Map

solution :: Solution ([Int], [Int]) [Int] Int
solution = Solution
  { decodeInput = fmap (over both sort . unzip) $ ((,) <$> decimal <*> (noeol *> decimal)) `sepBy` eol
  , solveA = Solver
    { solve = Just . uncurry (zipWith (\x y -> abs (x-y)))
    , display = show . sum
    }
  , solveB = defSolver
    { solve = Just . getSum . Map.foldMapWithKey (\k n -> Sum (k*n)) . uncurry countSimilarity
    }
  , tests =
    [ unlines
      [ "3   4"
      , "4   3"
      , "2   5"
      , "1   3"
      , "3   9"
      , "3   3"
      ] :=> [(PartA, "11"), (PartB, "31")]

    ]
  }

-- Note: problem statement is unclear on how to handle duplicates in left list.
-- Assuming they are to be counted multiple times.
countSimilarity :: [Int] -> [Int] -> Map Int Int
countSimilarity needles hay = Map.intersectionWith (*) (freq needles) (freq hay)
  where freq = getFreqs . toFreqMap