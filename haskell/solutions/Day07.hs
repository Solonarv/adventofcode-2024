{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import AOC.Solution
import ParsingPrelude
import Util

import Math.NumberTheory.Logarithms
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty, nonEmpty)

solution :: Solution [Equation] Int Int
solution = Solution
  { decodeInput = equationP `sepBy` eol
  , solveA = defSolver
    { solve = Just . sum . map eqnTarget . filter (canBeTrue [(+), (*)])
    }
  , solveB = defSolver
    { solve = Just . sum . map eqnTarget . filter (canBeTrue [(+), (*), concatIntegers])
    }
  , tests =
    [ unlines
      [ "190: 10 19"
      , "3267: 81 40 27"
      , "83: 17 5"
      , "156: 15 6"
      , "7290: 6 8 6 15"
      , "161011: 16 10 13"
      , "192: 17 8 14"
      , "21037: 9 7 18 13"
      , "292: 11 6 16 20"
      ] :=> [(PartA, "3749"), (PartB, "11387")]
    ]
  }

data Equation = Equation { eqnTarget :: !Int, eqnVals :: !(NonEmpty Int) }
  deriving (Eq, Ord, Show)

equationP :: Parser Equation
equationP = do
  target <- decimal
  ": "
  Just vals <- nonEmpty <$> (decimal `sepBy` noeol)
  pure $ Equation target vals

type IntOp = Int -> Int -> Int

allEvalPathsUpto :: Foldable1 t => [IntOp] -> Int -> t Int -> [Int]
allEvalPathsUpto ops bound = foldlM1 \x y -> do guard (x <= bound); op <- ops; pure $ op x y
{-# INLINE allEvalPathsUpto #-}

canBeTrue :: [IntOp] -> Equation -> Bool
canBeTrue ops (Equation target vals) = target `elem` allEvalPathsUpto ops target vals
-- canBeTrue ops (Equation target vals) = go (NonEmpty.reverse vals) target
--   where
--     go (NonEmpty.uncons -> (r', Nothing))  r = r == r'
--     go (NonEmpty.uncons -> (a, Just rest)) r = any (goOp a rest r) ops
--     goOp a rest r op
--       | Just b <- traceShow (op, r, a) $ invertOp op r a
--       = go rest b
--       | otherwise = False
{-# INLINE canBeTrue #-}

concatIntegers :: Int -> Int -> Int
concatIntegers = \ a b -> a * 10 ^ nDigits b + b
{-# INLINE concatIntegers #-}

nDigits :: Int -> Int
nDigits = (+1) . integerLog10 . fromIntegral  -- why no intLog10 :(
{-# INLINE nDigits #-}

-- stripSuffixInteger :: Integer -> Integer -> Maybe Integer
-- stripSuffixInteger x s = let
--   w_s = nDigits s
--   (h, t) = x `divMod` (10 ^ w_s)
--   in h <$ guard (t==0)

-- stripPrefixInteger :: Integer -> Integer -> Maybe Integer
-- stripPrefixInteger x p = let
--   w_p = nDigits p
--   w_x = nDigits x
--   (h, t) = x `divMod` (10 ^ (w_x - w_p))
--   ok = w_x >= w_p && p == h
--   in t <$ guard ok