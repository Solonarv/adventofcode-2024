{-# LANGUAGE OverloadedLabels, NoFieldSelectors, RecursiveDo #-}
module Day12 where

import AOC.Solution
import ParsingPrelude
import Grid2D
import Util

import Control.Lens
import Control.Lens.Unsound qualified as Unsound
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Set (Set)
import Data.Set qualified as Set
import Linear.Vector

import Data.Interval (Interval, Extended(..), Boundary(..), interval)
import Data.Interval qualified as Interval
import Data.IntervalSet (IntervalSet)
import Data.IntervalSet qualified as IntervalSet

solution :: Solution (Grid2D Char) Int Int
solution = Solution
  { decodeInput = fromLines <$> some letterChar `sepBy` eol
  , solveA = defSolver
    { solve = Just . \g -> getSum . foldMap regionPrice . view #regions $ execState (annotateRegions g) emptySt
    }
  , solveB = defSolver
    { solve = Just . \g -> getSum . foldMap regionPriceDiscounted . view #regions $ execState (annotateRegions g) emptySt
    }
  , tests =
    [ unlines
      [ "AAAA"
      , "BBCD"
      , "BBCC"
      , "EEEC"
      ] :=> [(PartA, "140"), (PartB, "80")]
    , unlines
      [ "OOOOO"
      , "OXOXO"
      , "OOOOO"
      , "OXOXO"
      , "OOOOO"
      ] :=> [(PartA, "772"), (PartB, "436")]
    , unlines
      [ "EEEEE"
      , "EXXXX"
      , "EEEEE"
      , "EXXXX"
      , "EEEEE"
      ] :=> [(PartB, "236")]
    , unlines
      [ "AAAAAA"
      , "AAABBA"
      , "AAABBA"
      , "ABBAAA"
      , "ABBAAA"
      , "AAAAAA"
      ] :=> [(PartB, "368")]
    , unlines
      [ "RRRRIICCFF"
      , "RRRRIICCCF"
      , "VVRRRCCFFF"
      , "VVRCCCJFFF"
      , "VVVVCJJCFE"
      , "VVIVCCJJEE"
      , "VVIIICJJEE"
      , "MIIIIIJJEE"
      , "MIIISIJEEE"
      , "MMMISSJEEE"
      ] :=> [(PartA, "1930"), (PartB, "1206")]
    ]
  }

data Region = Region
  { components :: !(Set (V2 Int))
  , leftEdges :: !(IntMap (IntervalSet Int))
  , topEdges :: !(IntMap (IntervalSet Int))
  -- , extent :: (V2 Int, V2 Int)
  }
  deriving (Eq, Ord, Show, Generic)

emptyRegion :: Region
emptyRegion = Region mempty mempty mempty

area :: Region -> Sum Int
area = Sum . Set.size . view #components

perimeter :: Region -> Sum Int
perimeter = summarizeEdges (sum . map (Sum . Interval.width))

longEdges :: Region -> Sum Int
longEdges = summarizeEdges (Sum . countHits ((>0) . Interval.width))

summarizeEdges :: Monoid w => ([Interval Int] -> w) -> Region -> w
summarizeEdges = foldMapOf
  $ Unsound.lensProduct #leftEdges #topEdges
  . both
  . folded
  . to IntervalSet.toList

singEdge :: Int -> IntervalSet Int
singEdge = IntervalSet.singleton . singInterval

singInterval :: Int -> Interval Int
singInterval (Finite -> x) = x Interval.<=..<= x + 1  -- using this instead of Interval.singleton ensured width and merging work correctly

insertConsecutive :: Int -> x -> IntMap x -> IntMap x
insertConsecutive k x = IntMap.insert k x . IntMap.insert (k+1) x

data St = St { regions :: IntMap Region, links :: IntMap Int, nextRegionId :: Int }
  deriving (Eq, Ord, Show, Generic)

emptySt :: St
emptySt = St IntMap.empty IntMap.empty 0

newRegion :: V2 Int -> State St Int
newRegion pos = do
  let
    r = addToRegion pos emptyRegion
  i <- #nextRegionId <+= 1
  #regions . at i ?= r
  pure i

toggleInterval :: Ord r => Interval r -> IntervalSet r -> IntervalSet r
toggleInterval ival env = if s (interior ival) `IntervalSet.isSubsetOf` env
  then IntervalSet.delete ival env
  else IntervalSet.insert ival env
  where
    s = IntervalSet.singleton

symmdiff :: Ord r => IntervalSet r -> IntervalSet r -> IntervalSet r
symmdiff a b = ab <> ba
  where
    ab = a `IntervalSet.difference` b
    ba = b `IntervalSet.difference` a

interior, closure :: Ord r => Interval r -> Interval r
interior r = Interval.interval (Interval.lowerBound r, Open) (Interval.upperBound r, Open)
closure r = Interval.interval (Interval.lowerBound r, Closed) (Interval.upperBound r, Closed)

addInterval :: Ord r => Interval r -> Maybe (IntervalSet r) -> Maybe (IntervalSet r)
addInterval ival Nothing = Just (IntervalSet.singleton ival)
addInterval ival (Just env) = Just (toggleInterval ival env)

addToRegion :: V2 Int -> Region -> Region
addToRegion pos r = let
  alreadyContained = r ^. #components . contains pos
  V2 x y = pos
  hasLeft = r ^. #components . contains (pos - unit _x)
  hasRight = r ^. #components . contains (pos + unit _x)
  hasAbove = r ^. #components . contains (pos - unit _y)
  hasBelow = r ^. #components . contains (pos + unit _y)
  bnd p = if p then Closed else Open
  hasNW = hasLeft || hasAbove
  hasNE = hasRight || hasAbove
  hasSW = hasLeft || hasBelow
  hasSE = hasRight || hasBelow
  leftEdge = interval (Finite y, bnd hasNW) (Finite y+1, bnd hasSW)
  rightEdge = interval (Finite y, bnd hasNE) (Finite y+1, bnd hasSE)
  topEdge = interval (Finite x, bnd hasNW) (Finite x+1, bnd hasNE)
  bottomEdge = interval (Finite x, bnd hasSW) (Finite x+1, bnd hasSE)
  in r &~ when (not alreadyContained) do
    -- traceShow (pos, leftEdge, rightEdge, topEdge, bottomEdge) $ pure ()
    #components . contains pos .= True
    zoom #leftEdges do
      at x %= addInterval leftEdge
      at (x+1) %= addInterval rightEdge
    zoom #topEdges do
      at y %= addInterval topEdge
      at (y+1) %= addInterval bottomEdge
      

resolveLink :: Int -> State St Int
resolveLink i = do
  end <- use (#links . at i) >>= \case
    Just i' -> do end <- resolveLink i'; pure end
    Nothing -> pure i
  when (end /= i) do
    #links . at i ?= end
  pure end

annotateRegions :: Grid2D Char -> State St (Grid2D Int)
annotateRegions grid = mdo
  result <- itraverse step grid
  let getInfo c pos = (grid ^? ix pos >>= guard . (== c)) *> (result ^? ix pos)
      step pos c =
        case (getInfo c (pos - unit _x), getInfo c (pos - unit _y)) of
          (Just i1_, Just i2_)
            -> do 
              i1 <- resolveLink i1_
              i2 <- resolveLink i2_
              r2 <- uses #regions (^?! ix i2)
              #regions . ix i1 %= addToRegion pos
              when (i1 /= i2) do
                #regions . ix i1 %= flip (foldr addToRegion) (r2 ^. #components)
                #regions . at i2 .= Nothing  -- BEGONE, you've been merged out of!
                #links . at i2 ?= i1
              pure i1
          (a, b)
            | Just r_ <- a <|> b
            -> do
              r <- resolveLink r_
              #regions . ix r %= addToRegion pos
              pure r
          _ -> newRegion pos
  -- regions <- use #regions
  -- trace (unlines $ showRegion grid <$> toList regions) $ pure ()
  pure result
        
regionPrice :: Region -> Sum Int
regionPrice r = area r * perimeter r

regionPriceDiscounted :: Region -> Sum Int
regionPriceDiscounted r = area r * longEdges r

showRegion :: Grid2D Char -> Region -> String
showRegion grid r = let
  somePlot = Set.findMin (r ^. #components)
  plantType = grid ^?! ix somePlot
  in unwords ["Region of", [plantType], "plants with price ", show $ area r, "*", show $ perimeter r]