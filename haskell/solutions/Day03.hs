{-# LANGUAGE OverloadedStrings #-}
module Day03 where

import AOC.Solution
import ParsingPrelude

import Control.Applicative (asum)

solution :: Solution [Instr] Int Int
solution = Solution
  { decodeInput = (anyInstr `scanManySkipping` anySingle) <* eof
  , solveA = defSolver { solve = Just . sum . map instrVal }
  , solveB = defSolver { solve = Just . evalInstrs }
  , tests =
      [ "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" :=> [(PartA, "161")]
      , "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" :=> [(PartB, "48")]
      ]
  }

data Instr = Mul !Int !Int | Do | Don't
  deriving (Eq, Ord, Show)

mulInstr :: Parser Instr
mulInstr = do
  "mul("
  a <- decimal
  ","
  b <- decimal
  ")"
  pure $ Mul a b

anyInstr :: Parser Instr
anyInstr = asum [Do <$ "do()", Don't <$ "don't()", mulInstr]

evalInstrs :: [Instr] -> Int
evalInstrs = go 0
  where
    go !acc (Mul a b : is) = go (acc + a * b) is
    go acc (Don't : is) = go acc (dropWhile (/= Do) is)
    go acc (Do : is) = go acc is
    go acc [] = acc

instrVal :: Instr -> Int
instrVal (Mul a b) = a * b
instrVal _ = 0