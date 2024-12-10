module Day06 where

import AOC.Solution
import ParsingPrelude
import Grid2D
import Util

import Control.Lens hiding (Empty)
import Data.Set (Set)
import Data.Set qualified as Set

solution :: Solution (Grid2D Tile, Guard) Int Int
solution = Solution
  { decodeInput = do
      grid <- Grid2D.fromLines <$> (some (oneOf ".#^v<>") `sepBy` eol)
      (pos, c) <- maybe (fail "no guard!") pure $ ifind (\_ c -> c `elem` "^v<>") grid
      let dir = fromMaybe (error "impossible") $ readDirection c
      pure (readTile <$> grid, Guard pos dir)
  , solveA = defSolver
    { solve = \(lab, g) -> Just . Set.size . Set.map guardPos . snd $ guardStates lab g
    }
  , solveB = defSolver
    { solve = \(lab, g) -> Just . countHits (isLoop g) $ loopCandidates lab g
    }
  , tests =
    [ unlines
      [ "....#....."
      , ".........#"
      , ".........."
      , "..#......."
      , ".......#.."
      , ".........."
      , ".#..^....."
      , "........#."
      , "#........."
      , "......#..."
      ] :=> [(PartA, "41"), (PartB, "6")]
    ]
  }

data Tile = Empty | Obstacle
  deriving (Eq, Ord, Show)

readTile :: Char -> Tile
readTile '#' = Obstacle
readTile _ = Empty

data Direction = R | D | L | U
  deriving (Eq, Ord, Show, Enum)

nextDir :: Direction -> Direction
nextDir = \case
  U -> R
  x -> succ x

dir2unit :: Direction -> V2 Int
dir2unit = \case
  U -> V2 0 (-1)
  D -> V2 0 1
  L -> V2 (-1) 0
  R -> V2 1 0

readDirection :: Char -> Maybe Direction
readDirection = \case
  '^' -> Just U
  'v' -> Just D
  '<' -> Just L
  '>' -> Just R
  _ -> Nothing

data Guard = Guard { guardPos :: !(V2 Int), guardDir :: !Direction }
  deriving (Eq, Ord, Show)

stepGuard :: Grid2D Tile -> Guard -> Maybe Guard
stepGuard lab (Guard pos dir) = let
  fw = pos + dir2unit dir
  in case lab ^? ix fw of
      Just Empty -> Just $ Guard fw dir
      Just Obstacle -> Just $ Guard pos (nextDir dir)
      Nothing -> Nothing

guardStates :: Grid2D Tile -> Guard -> (Bool, Set Guard)
guardStates lab = go Set.empty
  where
    go seen g
      | g `Set.member` seen = (True, seen)
      | Nothing <- next = (False, seen')
      | Just g' <- next = go seen' g'
      where next = stepGuard lab g; seen' = Set.insert g seen

isLoop :: Guard -> Grid2D Tile -> Bool
isLoop g lab = fst $ guardStates lab g

-- not very clever right now, just tries all tiles the guard visits
loopCandidates :: Grid2D Tile -> Guard -> [Grid2D Tile]
loopCandidates lab g =
  [ lab & ix pos .~ Obstacle
  | pos <- Set.toList . Set.map guardPos . snd $ guardStates lab g
  , pos /= guardPos g
  ]