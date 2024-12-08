{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import AOC.Solution
import ParsingPrelude

import Math.NumberTheory.Logarithms
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty, nonEmpty)

solution :: Solution [Equation] Integer Integer
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

data Equation = Equation { eqnTarget :: !Integer, eqnVals :: NonEmpty Integer }
  deriving (Eq, Ord, Show)

equationP :: Parser Equation
equationP = do
  target <- decimal
  ": "
  Just vals <- nonEmpty <$> (decimal `sepBy` noeol)
  pure $ Equation target vals

type IntegerOp = Integer -> Integer -> Integer

allEvalPaths :: Foldable1 t => [IntegerOp] -> t Integer -> [Integer]
allEvalPaths ops = foldlM1 \x y -> do op <- ops; pure $ op x y
{-# INLINE allEvalPaths #-}

canBeTrue :: [IntegerOp] -> Equation -> Bool
canBeTrue ops (Equation target vals) = target `elem` allEvalPaths ops vals
{-# INLINE canBeTrue #-}

concatIntegers :: IntegerOp
concatIntegers a b = a * 10 ^ w + b
  where w = integerLog10 b +1
{-# INLINE concatIntegers #-}