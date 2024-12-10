module Grid2D (module Grid2D, module Linear.V2) where

import Data.Maybe

import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Linear.V2

import Util

data Grid2D a = Grid2D !Int !Int !(Vector a)
  deriving (Eq, Ord, Foldable, Functor, Traversable)

width, height :: Grid2D a -> Int
width (Grid2D w _ _) = w
height (Grid2D _ h _) = h

type instance Index (Grid2D a) = V2 Int
type instance IxValue (Grid2D a) = a
instance Ixed (Grid2D a) where
  ix (V2 x y) = gridPoint x y

instance FunctorWithIndex (V2 Int) Grid2D where
  imap = iover itraversed

instance FoldableWithIndex (V2 Int) Grid2D where
  ifoldMap = ifoldMapOf itraversed
  ifoldr = ifoldrOf itraversed

instance TraversableWithIndex (V2 Int) Grid2D where
  itraverse f (Grid2D w h vec) = Grid2D w h <$> itraverseVec (f . unflatten) vec
    where
      unflatten i = let (y, x) = i `divMod` w in V2 x y
      itraverseVec = itraverseOf (indexing traversed)

gridPoint :: Int -> Int -> Traversal' (Grid2D a) a
gridPoint x y f g@(Grid2D w h vec)
  | x >= w || y >= h || x < 0 || y < 0
  = pure g
  | otherwise
  = let i = y*w + x
    in f (vec Vector.! i) <&> \a -> Grid2D w h (vec Vector.// [(i, a)])

gridPointWrap :: Int -> Int -> Traversal' (Grid2D a) a
gridPointWrap x y f (Grid2D w h vec) = let
  i = (y `mod` h)*w + x `mod` w
  in f (vec Vector.! i) <&> \a -> Grid2D w h (vec Vector.// [(i, a)])

fromLines :: [[a]] -> Grid2D a
fromLines xss = let
  h = length xss
  w = sum' (length <$> listToMaybe xss)
  vec = Vector.fromList (concat xss)
  in Grid2D w h vec

adjacents :: Int -> Int -> Grid2D a -> [a]
adjacents x y grid = mapMaybe (grid ^?)
  [ gridPoint (x+dx) (y+dy)
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , dx /= 0 || dy /= 0
  ]

adjacentsWithCoords :: Int -> Int -> Grid2D a -> [(V2 Int, a)]
adjacentsWithCoords x y grid =
  [ (V2 x' y', val)
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , dx /= 0 || dy /= 0
  , let x' = x + dx
  , let y' = y + dy
  , val <- grid ^.. gridPoint x' y'
  ]

adjacentsNeumann :: Int -> Int -> Grid2D a -> [a]
adjacentsNeumann x y grid = catMaybes
  [ grid ^? gridPoint (x-1) y
  , grid ^? gridPoint (x+1) y
  , grid ^? gridPoint x (y-1)
  , grid ^? gridPoint x (y+1)
  ]

adjacentsNeumannWithCoords :: Int -> Int -> Grid2D a -> [(V2 Int, a)]
adjacentsNeumannWithCoords x y grid = catMaybes
  [ (V2 (x-1) y,) <$> grid ^? gridPoint (x-1) y
  , (V2 (x+1) y,) <$> grid ^? gridPoint (x+1) y
  , (V2 x (y-1),) <$> grid ^? gridPoint x (y-1)
  , (V2 x (y+1),) <$> grid ^? gridPoint x (y+1)
  ]

ray :: Int -> Int -> Int -> Int -> Grid2D a -> [a]
ray sx sy dx dy grid = go sx sy
  where
    go x y = case grid ^? gridPoint x y of
      Nothing -> []
      Just t -> t : go (x+dx) (y+dy)

rayWithCoords :: Int -> Int -> Int -> Int -> Grid2D a -> [(V2 Int, a)]
rayWithCoords sx sy dx dy grid = go sx sy
  where
    go x y = case grid ^? gridPoint x y of
      Nothing -> []
      Just t -> (V2 x y, t) : go (x+dx) (y+dy)

allRays :: Int -> Int -> Grid2D a -> [[a]]
allRays dx dy g =
  [ ray sx sy dx dy g
  | sx <- [0 .. width g - 1]
  , sy <- [0 .. height g  - 1]
  ]

allRaysWithCoords :: Int -> Int -> Grid2D a -> [[(V2 Int, a)]]
allRaysWithCoords dx dy g =
  [ rayWithCoords sx sy dx dy g
  | sx <- [0 .. width g - 1]
  , sy <- [0 .. height g  - 1]
  ]

genGrid :: Int -> Int -> (Int -> Int -> a) -> Grid2D a
genGrid w h f = Grid2D w h entries
  where
    entries = Vector.generate (w*h) \i ->
      let (y, x) = i `divMod` w
      in f x y

showCharGrid :: (a -> Char) -> Grid2D a -> String
showCharGrid f (Grid2D w h grid) = unlines
  [ [ f (grid Vector.! (y*w+x))
    | x <- [0 .. w-1]
    ]
  | y <- [0 .. h-1]
  ]

rows :: Grid2D a -> Vector (Vector a)
rows (Grid2D w h v) = Vector.generate h \y -> Vector.slice (y*w) w v

cols :: Grid2D a -> Vector (Vector a)
cols (Grid2D w h v) = Vector.generate w \x -> Vector.generate h \y ->
  v Vector.! (y*w + x)
