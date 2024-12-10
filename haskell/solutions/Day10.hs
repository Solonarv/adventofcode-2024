module Day10 where

import AOC.Solution
import ParsingPrelude
import Grid2D

import Control.Lens
import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as Set

solution :: Solution (Grid2D Int) Int Int
solution = Solution
  { decodeInput = fromLines <$> (some tileP `sepBy` eol)
  , solveA = defSolver
    { solve = Just . trailheadScores
    }
  , solveB = defSolver
    { solve = Just . trailheadRatings
    }
  , tests =
    [ unlines
      [ "...0..."
      , "...1..."
      , "...2..."
      , "6543456"
      , "7.....7"
      , "8.....8"
      , "9.....9"
      ] :=> [(PartA, "2")]
    , unlines
      [ "..90..9"
      , "...1.98"
      , "...2..7"
      , "6543456"
      , "765.987"
      , "876...."
      , "987...."
      ] :=> [(PartA, "4")]
    , unlines
      [ "10..9.."
      , "2...8.."
      , "3...7.."
      , "4567654"
      , "...8..3"
      , "...9..2"
      , ".....01"
      ] :=> [(PartA, "3")]
    , unlines
      [ "89010123"
      , "78121874"
      , "87430965"
      , "96549874"
      , "45678903"
      , "32019012"
      , "01329801"
      , "10456732"
      ] :=> [(PartA, "36"), (PartB, "81")]
    , unlines
      [ ".....0."
      , "..4321."
      , "..5..2."
      , "..6543."
      , "..7..4."
      , "..8765."
      , "..9...."
      ] :=> [(PartB, "3")]
    , unlines
      [ "..90..9"
      , "...1.98"
      , "...2..7"
      , "6543456"
      , "765.987"
      , "876...."
      , "987...."
      ] :=> [(PartB, "13")]
    , unlines
      [ "012345"
      , "123456"
      , "234567"
      , "345678"
      , "4.6789"
      , "56789."
      ] :=> [(PartB, "227")]
    ]
  }

tileP :: Parser Int
tileP = 99 <$ single '.' <|> singleDigit 10
  -- quick hack: "impassable" . tiles from the examples get processed as height 99

reachablePeaks :: Grid2D Int -> Grid2D (Set (V2 Int))
reachablePeaks topo = scores
  where
    scores = imap score topo
    score pos 9 = Set.singleton pos
    score (V2 x y) h = Set.unions [scores ^?! ix pos | (pos, h') <- adjacentsNeumannWithCoords x y topo, h' == h+1]

trailheadScores :: Grid2D Int -> Int
trailheadScores topo = getSum $ ifoldMap scoreHead topo
  where
    scores = reachablePeaks topo
    scoreHead pos h
      | h /= 0 = 0
      | otherwise = Sum . Set.size $ scores ^?! ix pos

pathsToPeaks :: Grid2D Int -> Grid2D Int
pathsToPeaks topo = paths
  where
    paths = imap count topo
    count _ 9 = 1
    count (V2 x y) h = sum [paths ^?! ix pos | (pos, h') <- adjacentsNeumannWithCoords x y topo, h' == h+1]

trailheadRatings :: Grid2D Int -> Int
trailheadRatings topo = getSum $ ifoldMap rateHead topo
  where
    paths = pathsToPeaks topo
    rateHead pos h
      | h /= 0 = 0
      | otherwise = Sum $ paths ^?! ix pos