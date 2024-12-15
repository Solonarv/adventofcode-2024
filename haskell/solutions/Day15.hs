{-# LANGUAGE NoFieldSelectors, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Day15 where

import AOC.Solution
import ParsingPrelude
import Util
import Grid2D

import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Control.Lens
import Data.Set (Set)
import Data.Set qualified as Set

solution :: Solution (Warehouse, [Direction]) Int Int
solution = Solution
  { decodeInput = do
      tiles <- fromLines <$> some tileP `sepEndBy` eol
      eol
      moves <- concat <$> some moveP `sepBy` space1
      pure (tilesToWarehouse tiles, moves)
  , solveA = defSolver
    { solve = Just . sumBoxGPS . \(wh, moves) -> foldl' (flip performMove) wh moves
    }
  , solveB = defSolver
    { solve = Just . sumBoxGPS . \(widen -> wh, moves) -> foldl' (flip performWideMove) wh moves
    }
  , tests =
    [ unlines
      [ "########"
      , "#..O.O.#"
      , "##@.O..#"
      , "#...O..#"
      , "#.#.O..#"
      , "#...O..#"
      , "#......#"
      , "########"
      , ""
      , "<^^>>>vv<v>>v<<"
      ] :=> [(PartA, "2028")]
    , unlines
      [ "##########"
      , "#..O..O.O#"
      , "#......O.#"
      , "#.OO..O.O#"
      , "#..O@..O.#"
      , "#O#..O...#"
      , "#O..O..O.#"
      , "#.OO.O.OO#"
      , "#....O...#"
      , "##########"
      , ""
      , "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
      , "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
      , "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
      , "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
      , "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
      , "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
      , ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
      , "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
      , "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
      , "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
      ] :=> [(PartA, "10092"), (PartB, "9021")]
    , unlines
      [ "#######"
      , "#...#.#"
      , "#.....#"
      , "#..OO@#"
      , "#..O..#"
      , "#.....#"
      , "#######"
      , ""
      , "<vv<<^^<<^^"
      ] :=> []
    ]
  }

data Tile = TWall | TEmpty | TBox | TRobot
  deriving (Eq, Ord, Show)

tileP :: Parser Tile
tileP = asum
  [ TWall <$ "#"
  , TEmpty <$ "."
  , TBox <$ "O"
  , TRobot <$ "@"
  ]

data Direction
 = U | D | L | R
  deriving (Eq, Ord, Show)

moveP :: Parser Direction
moveP = asum
  [ U <$ "^"
  , D <$ "v"
  , L <$ "<"
  , R <$ ">"
  ]

v2Of :: Direction -> V2 Int
v2Of = \case
  U -> V2 0 (-1)
  D-> V2 0 1
  L-> V2 (-1) 0
  R -> V2 1 0

data Warehouse = Warehouse
  { robot :: V2 Int,
  walls :: Set (V2 Int),
  boxes :: Set (V2 Int)    -- boxes; or, in part 2, the left halves of boxes
  }
  deriving (Eq, Show, Ord, Generic)

insertTile :: V2 Int -> Warehouse -> Tile -> Warehouse
insertTile pos = flip \case
  TWall -> #walls . contains pos .~ True
  TBox -> #boxes . contains pos .~ True
  TRobot -> #robot .~ pos
  TEmpty -> id

tilesToWarehouse :: Grid2D Tile -> Warehouse
tilesToWarehouse = ifoldl' insertTile
  Warehouse
  { robot = error "missing robot!"
  , walls = mempty
  , boxes = mempty
  }

performMove :: Direction -> Warehouse -> Warehouse
performMove (v2Of -> dir) wh = let
  newbot = wh ^. #robot + dir
  in if wh ^. #walls . contains newbot
    then wh
    else case shoveBoxes dir newbot wh of
      Just final -> wh &~ do
        #robot .= newbot
        #boxes . contains final .= True
        #boxes . contains newbot .= False
      Nothing -> wh

-- | if boxes are pushed in @dir@ starting from @pos@,
-- where does the final box end up?
-- Nothing means there is no space for the final box (i.e. ran into a wall)
shoveBoxes :: V2 Int -> V2 Int -> Warehouse -> Maybe (V2 Int)
shoveBoxes dir pos wh
  | wh ^. #walls . contains pos = Nothing
  | wh ^. #boxes . contains pos = shoveBoxes dir (pos + dir) wh
  | otherwise = Just pos

sumBoxGPS :: Warehouse -> Int
sumBoxGPS = sumOf $ #boxes . folded . to toGPS
  where
    toGPS (V2 x y) = x + 100*y

widen :: Warehouse -> Warehouse
widen Warehouse{..} = Warehouse
  { robot = dbl robot
  , walls = liftA2 Set.union (Set.mapMonotonic dbl) (Set.mapMonotonic dbl1) walls
  , boxes = Set.mapMonotonic dbl boxes
  }
  where
    dbl = (V2 2 1 *)
    dbl1 = (V2 1 0 +) . dbl

performWideMove :: Direction -> Warehouse -> Warehouse
performWideMove dir wh = let
  v = v2Of dir
  newbot = wh ^. #robot + v
  in if wh ^. #walls . contains newbot
    then wh
    else case boxesToMove dir newbot wh of
      Just changes -> wh &~ do
        #robot .= newbot
        #boxes %= applyBoxMovements dir changes
      Nothing -> wh

boxesToMove :: Direction -> V2 Int -> Warehouse -> Maybe (Set (V2 Int))
boxesToMove dir pos wh
  | wh ^. #walls . contains pos = Nothing
  | wh ^. #boxes . contains pos = shove pos
  | wh ^. #boxes . contains posL = shove posL
  | otherwise = Just Set.empty
  where
    -- if pos contains the right half of a box, posL contains the corresponding left half
    posL = pos - v2Of R
    shove p = case dir of
      L -> Set.insert p <$> boxesToMove dir (p + v2Of dir) wh
      R -> Set.insert p <$> boxesToMove dir (p + 2*v2Of dir) wh
      _ -> do
        movingL <- boxesToMove dir (p + v2Of dir) wh
        movingR <- boxesToMove dir (p + v2Of dir + v2Of R) wh
        pure $ Set.insert p $ Set.union movingL movingR

applyBoxMovements :: Direction -> Set (V2 Int) -> Set (V2 Int) -> Set (V2 Int)
applyBoxMovements dir chk boxes = unaffected <> moved
  where
    unaffected = boxes `Set.difference` chk
    affected = boxes `Set.intersection` chk
    moved = Set.mapMonotonic (+ v2Of dir) affected

showWideWarehouse :: Warehouse -> String
showWideWarehouse wh = showCharGrid id $ genGrid w h spot
  where
    V2 w h = Set.findMax (wh ^. #walls) + 1
    spot x y
      | v == wh^. #robot = '@'
      | wh ^. #walls.contains v = '#'
      | wh ^. #boxes.contains v = '['
      | wh ^. #boxes.contains (v-v2Of R) = ']'
      | otherwise = '.'
      where v = V2 x y