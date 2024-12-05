{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Lazy qualified as LazyIntMap
import Data.IntSet qualified as IntSet
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Tim as Tim

import Debug.Trace

solution :: Solution Orders [Vector Int] [Vector Int]
solution = Solution
  { decodeInput = ordersP
  , solveA = Solver
    { solve = \(Orders rules runs) -> Just $ filter (wellOrdered rules) runs
    , display = show . sum . map middleEntry
    }
  , solveB = Solver
    { solve = \(Orders rules runs) -> Just . map (Vector.modify $ Tim.sortBy (rules2Ord rules)) . filter (not . wellOrdered rules) $ runs
    , display = show . sum . map middleEntry
    }
  , tests =
    [ unlines
      [ "47|53"
      , "97|13"
      , "97|61"
      , "97|47"
      , "75|29"
      , "61|13"
      , "75|53"
      , "29|13"
      , "97|29"
      , "53|29"
      , "61|53"
      , "97|53"
      , "61|29"
      , "47|13"
      , "75|47"
      , "97|75"
      , "47|61"
      , "75|61"
      , "47|29"
      , "75|13"
      , "53|13"
      , ""
      , "75,47,61,53,29"
      , "97,61,53,29,13"
      , "75,29,13"
      , "75,97,47,61,53"
      , "61,13,29"
      , "97,13,75,29,47"
      ] :=> [(PartA, "143"), (PartB, "123")]
    ]
  }

middleEntry :: Vector a -> a
middleEntry v = v ! (length v `div` 2)

data Orders = Orders !(IntMap [Int]) [Vector Int]
  deriving (Eq, Ord, Show)

ruleP :: Parser (Int, [Int])
ruleP = sing <$> decimal <* "|" <*> decimal
  where sing k v = (k, [v])

ordersP :: Parser Orders
ordersP = do
  rules <- IntMap.fromListWith (++) <$> (ruleP `sepEndBy` eol)
  eol
  runs <- fmap Vector.fromList (decimal `sepBy` ",") `sepBy` eol
  pure $ Orders rules runs

wellOrdered :: IntMap [Int] -> Vector Int -> Bool
wellOrdered rules v = go 0 IntSet.empty
  where
    go ix seen
      | ix == length v = True
      | Just later <- IntMap.lookup here rules
      , any (`IntSet.member` seen) later = False
      | otherwise = go (ix+1) (IntSet.insert here seen)
      where here = v ! ix

-- I'm surprised that this works without taking a transitive closure first...
-- looks like the input is already transitively closed
rules2Ord :: IntMap [Int] -> (Int -> Int -> Ordering)
rules2Ord rules a b
  | a == b = EQ
  | Just afterA <- IntMap.lookup a rules
  , b `elem` afterA = LT
  | Just afterB <- IntMap.lookup b rules
  , a `elem` afterB = GT
  | otherwise = EQ