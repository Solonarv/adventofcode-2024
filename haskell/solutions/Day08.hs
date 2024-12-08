module Day08 where

import AOC.Solution
import ParsingPrelude
import Grid2D
import Util

import Control.Lens hiding (Empty)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.List
import Data.Set qualified as Set
import Linear.V2
import Linear.Vector

solution :: Solution (Grid2D Tile) Int Int
solution = Solution
  { decodeInput = fromLines <$> (some readTile `sepBy` eol)
  , solveA = defSolver
    { solve = \g -> Just . Set.size . foldMap (Set.fromList  . filter (inBounds g) . antinodes) . collectAntennas $ g
    }
  , solveB = defSolver
    { solve = \g -> Just . Set.size . foldMap (Set.fromList . resonantAntinodes (inBounds g)) . collectAntennas $ g
    }
  , tests =
    [ unlines
      [ ".........."
      , "...#......"
      , ".........."
      , "....a....."
      , ".........."
      , ".....a...."
      , ".........."
      , "......#..."
      , ".........."
      , ".........."
      ] :=> [(PartA, "2")]
    , unlines
      [ "T....#...."
      , "...T......"
      , ".T....#..."
      , ".........#"
      , "..#......."
      , ".........."
      , "...#......"
      , ".........."
      , "....#....."
      , ".........."
      ] :=> [(PartB, "9")]
    , unlines
      [ "............"
      , "........0..."
      , ".....0......"
      , ".......0...."
      , "....0......."
      , "......A....."
      , "............"
      , "............"
      , "........A..."
      , ".........A.."
      , "............"
      , "............"
      ] :=> [(PartA, "14"), (PartB, "34")]
    ]
  }

data Tile = Empty | Antenna !Char
  deriving (Eq, Ord, Show)

readTile :: Parser Tile
readTile = (Empty <$ (oneOf ".#")) <|> (Antenna <$> alphaNumChar)

collectAntennas :: Grid2D Tile -> Map Char [V2 Int]
collectAntennas = ifoldl upd Map.empty
  where
    upd _ coll Empty = coll
    upd (x,y) coll (Antenna f) = Map.insertWith (++) f [V2 x y] coll

antinodes :: [V2 Int] -> [V2 Int]
antinodes antennas =
  [ node
  | (p : rest) <- tails antennas
  , q <- rest
  , node <- [2*p-q, 2*q-p]
  ]

inBounds :: Grid2D a -> V2 Int -> Bool
inBounds g node =
  (within 0 (width g - 1) $ node ^. _x)
  && (within 0 (height g - 1) $ node ^. _y)

resonantAntinodes :: (V2 Int -> Bool) -> [V2 Int] -> [V2 Int]
resonantAntinodes isOk antennas =
  [ node
  | (p : rest) <- tails antennas
  , q <- rest
  , let d = p - q
  , let forward = takeWhile isOk [q + n *^ d | n <- [0..]]
  , let backward = takeWhile isOk [q - n *^ d | n <- [1..]]
  , node <- forward ++ backward
  ]