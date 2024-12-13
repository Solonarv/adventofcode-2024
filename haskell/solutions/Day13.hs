{-# LANGUAGE NoFieldSelectors, RecordWildCards, OverloadedStrings #-}
module Day13 where

import Prelude hiding (Rational)

import AOC.Solution
import ParsingPrelude
import Util

import Linear
import Data.Ratio hiding (Rational)  -- we using Ratio Int here

solution :: Solution [ClawMachine] Int Int
solution = Solution
  { decodeInput = clawMachineP `sepBy` do eol; eol
  , solveA = defSolver
    { solve = Just . sum . map scoreMoves . mapMaybe winningMoves
    }
  , solveB = defSolver
    { solve = Just . sum . map scoreMoves . mapMaybe (winningMoves . embiggen)
    }
  , tests =
    [ unlines
      [ "Button A: X+94, Y+34"
      , "Button B: X+22, Y+67"
      , "Prize: X=8400, Y=5400"
      ] :=> [(PartA, "280")]
    , unlines
      [ "Button A: X+26, Y+66"
      , "Button B: X+67, Y+21"
      , "Prize: X=12748, Y=12176"
      , ""
      , "Button A: X+69, Y+23"
      , "Button B: X+27, Y+71"
      , "Prize: X=18641, Y=10279"
      ] :=> [(PartA, "0")]  -- second and fourth claw machine from the example
    ]
  }

data ClawMachine = ClawMachine { btnA :: V2 Int, btnB :: V2 Int, prize :: V2 Int }
  deriving (Eq, Ord, Show)

clawMachineP :: Parser ClawMachine
clawMachineP = do
  "Button A: "
  btnA <- vec2P
  eol; "Button B: "
  btnB <- vec2P
  eol; "Prize: "
  prizeX <- do "X="; decimal
  prizeY <- do ", Y="; decimal
  let prize = V2 prizeX prizeY
  pure ClawMachine{..}

vec2P :: Parser (V2 Int)
vec2P = do
  "X+"; x <- decimal
  ", "
  "Y+"; y <- decimal
  pure $ V2 x y

type Rational = Ratio Int

toRat :: Int -> Rational
toRat = (% 1)

asNonnegInt :: Rational -> Maybe Int
asNonnegInt x = n <$ guard (d == 1 && n >= 0)
  where
    n = numerator x
    d = denominator x

winningMoves :: ClawMachine -> Maybe (V2 Int)
winningMoves ClawMachine{..} = let
  btns = Linear.transpose $ (fmap.fmap) toRat $ V2 btnA btnB
  goal = fmap toRat prize
  moves = inv22 btns !* goal
  in traverse asNonnegInt moves

scoreMoves :: V2 Int -> Int
scoreMoves (V2 a b) = 3 * a + b

-- this is all I need for part 2 lol
embiggen :: ClawMachine -> ClawMachine
embiggen cm@ClawMachine{..} = cm { prize = prize + 10000000000000 }