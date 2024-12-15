{-# LANGUAGE NoFieldSelectors, OverloadedLabels, RecordWildCards, OverloadedStrings #-}
module Day14 where

import AOC.Solution
import AOC.InteractiveHelpers  -- :(
import ParsingPrelude
import Util
import DynMap
import Grid2D

import Linear
import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Control.Lens
import Data.Ord

import Control.Concurrent
import System.Environment (getArgs)

solution :: Solution [Bot] Int ()
solution = Solution
  { decodeInput = botP `sepBy` eol
  , solveA = defSolver
    { solve = Just . scoreQuadrants . map (moveBot 100)
    }
  , solveB = Solver
    { solve = const $ Just ()
    , display = const "must do manual inspection"
    }
  , tests =
    [ WithDyn "dimensions" (V2 @Int 11 7)
    $ unlines
      [ "p=0,4 v=3,-3"
      , "p=6,3 v=-1,-3"
      , "p=10,3 v=-1,2"
      , "p=2,0 v=2,-1"
      , "p=0,0 v=1,3"
      , "p=3,0 v=-2,-2"
      , "p=7,6 v=-1,-3"
      , "p=3,0 v=-1,-2"
      , "p=9,3 v=2,3"
      , "p=7,3 v=-1,2"
      , "p=2,4 v=2,-3"
      , "p=9,5 v=-3,-3"
      ] :=> [(PartA, "12")]
    ]
  }

dimensions :: HasDyns => V2 Int
dimensions = getDyn "dimensions" (V2 101 103)

wrap :: HasDyns => V2 Int -> V2 Int
wrap r = liftA2 mod r dimensions

data Bot = Bot { pos :: !(V2 Int), vel :: !(V2 Int) }
  deriving (Eq, Ord, Show, Generic)

botP :: Parser Bot
botP = do
  "p="
  pos <- v2
  " v="
  vel <- v2
  pure $ Bot{..}
  where
    v2 :: Parser (V2 Int)
    v2 = liftA2 V2
        do signed mempty decimal
        do ","; signed mempty decimal


moveBot :: HasDyns => Int -> Bot -> V2 Int
moveBot d Bot{..} = wrap $ pos + d *^ vel

scoreQuadrants :: HasDyns => [V2 Int] -> Int
scoreQuadrants = getSum . product . foldMap toQuadrant
  where
    V2 w h = dimensions
    x0 = w `div` 2
    y0 = h `div` 2
    toQuadrant (V2 x y)
      | x > x0, y > y0 = V4 1 0 0 0
      | x > x0, y < y0 = V4 0 1 0 0
      | x < x0, y < y0 = V4 0 0 1 0
      | x < x0, y > y0 = V4 0 0 0 1
      | otherwise = 0

-- Part 2
-- first off, this is bullshit
-- why are you making me look at the outputs!!!
-- cant you just give me a nice computable specification of what a christmas tree is, come on
-- I bet you use the same tree for everyone anyway!

-- heuristic:
-- very high score indicates very even spread of bots (big xmas tree)
-- very low score indicates clustering in one or two quadrants (small xmas tree)
isLikelyXmasTree :: HasDyns => [V2 Int] -> Bool
isLikelyXmasTree bots =
  score < maximalScore `div` 4
  || score > 10 * maximalScore `div` 10
  where
  maximalScore = (500 `div` 4)^4
  score = scoreQuadrants bots

toMap :: HasDyns => [V2 Int] -> Grid2D Int
toMap = foldl' addBot $ genGrid w h (\_ _ -> 0)
  where
    V2 w h = dimensions
    addBot g pos = g & ix pos +~ 1

showMap :: Grid2D Int -> String
showMap = showCharGrid tile
  where
    tile 0 = '.'
    tile x
      | x < 10 = toEnum (x + fromEnum '0')
      | otherwise = '*'

readInput :: IO [Bot]
readInput = do
  let filename = "input/day14.txt"
  contents <- readFile filename
  either die' pure $ parseNicely filename (decodeInput solution) contents

run :: Int -> [Bot] -> IO ()
run threshold bots = let ?dyns = emptyDynMap in let
  poss = sortBy (comparing (view _3)) [(t, grid, scoreQuadrants grid) | t <- [0 .. 10102], let grid = map (moveBot t) bots]
  candidates = take threshold poss
  in for_ candidates \(t, grid, _) -> do
    putStrLn $ showMap $ toMap grid
    putStrLn $ "\n TIME: " <> show t

-- some clustering on period 68
-- not helpful though
-- maybe try really low scoring grids?

simpleMain :: IO ()
simpleMain = do
  [threshold] <- map read <$> getArgs
  readInput >>= run threshold