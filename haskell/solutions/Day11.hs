module Day11 where

import AOC.Solution
import ParsingPrelude
import Util

import Math.NumberTheory.Logarithms
import Data.MemoTrie

solution :: Solution [Int] Int Int
solution = Solution
  { decodeInput = decimal `sepBy` noeol
  , solveA = defSolver
    { solve = Just . sum . map (stones 25)
    }
  , solveB = defSolver
    { solve = Just . sum . map (stones 75)
    }
  , tests =
    [ "125 17" :=> [(PartA, "55312")]
    ]
  }

blink :: Int -> [Int]
blink 0 = [1]
blink n
  | (w, 0) <- nDigits n `divMod` 2
  , (p, q) <- n `divMod` (10^w)
  = [p, q]
  | otherwise = [n * 2024]

-- memoIntFunction :: (Int -> a) -> Int -> a
-- memoIntFunction f = f'
--   where
--     f' a = unsafePerformIO do
--       cache <- readIORef cacheRef
--       case IntMap.lookup a cache of
--         Just b -> pure b
--         Nothing -> do
--           let b = f a
--           modifyIORef cacheRef (IntMap.insert a b)
--           pure b
--     cacheRef :: IORef (IntMap a)
--     cacheRef = unsafePerformIO (newIORef IntMap.empty)
--     {-# NOINLINE cacheRef #-}

stones :: Int ->  Int -> Int
stones = curry $ memoFix \self -> \case
  (0, _) -> 1
  (s, n)
    | 0 == n
    -> self (s-1, 1)
    | (w, 0) <- nDigits n `divMod` 2
    , (p, q) <- n `divMod` (10^w)
    -> self (s-1, p) + self (s-1, q)
    | otherwise -> self (s-1, n*2024)

nDigits :: Int -> Int
nDigits = (+1) . integerLog10 . fromIntegral  -- why no intLog10 :(
{-# INLINE nDigits #-}