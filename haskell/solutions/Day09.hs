module Day09 where

import AOC.Solution
import ParsingPrelude
import Util

import Control.Lens hiding (Empty, inside)
import GHC.Generics (Generic)
import Data.Generics.Labels
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq

solution :: Solution Disk Disk Disk
solution = Solution
  { decodeInput = augmentRLE <$> some digitP
  , solveA = Solver
    { solve = Just . fragmentDisk
    , display = show . checksum
    }
  , solveB = Solver
    { solve = Just . compact
    , display = show . checksum
    }
  , tests =
    [ "2333133121414131402"
      :=> [(PartA, "1928"), (PartB, "2858")]
    ]
  }

digitP :: Parser Int
digitP = do
  Just d <- char2digitBase 10 <$> digitChar
  pure d

newtype Disk = Disk { diskChunks :: Seq Chunk }
  deriving (Eq, Ord, Show)

data Chunk = Chunk { chunkFileId :: Maybe Int, chunkSize :: Int }
  deriving (Eq, Ord, Show, Generic)

isEmpty :: Chunk -> Bool
isEmpty = isNothing . chunkFileId

emptyChunk :: Int -> Chunk
emptyChunk = Chunk Nothing

augmentRLE :: [Int] -> Disk
augmentRLE xs = Disk { diskChunks = chunks}
  where
    chunks = Seq.fromList . snd . mapAccumL augmentChunk (0, True) $ xs
    augmentChunk (!fid, True) !sz = ((fid + 1, False), Chunk (Just fid) sz)
    augmentChunk (fid, False) sz = ((fid, True), Chunk Nothing sz)
    -- this could be written as mapAccum{L,R} (not sure which)
    -- followed by concat, but this is easier

showFileMap :: Disk -> String
showFileMap = ifoldMap showChunk . diskChunks
  where
    showChunk _ chk = concat $ replicate (chunkSize chk) (showId (chunkFileId chk))
    showId = maybe (".") show

-- shuffle one partial file into the first free space.
-- returns Nothing if there is nothing to do (no free space).
shuffleFragment :: Disk -> Maybe Disk
shuffleFragment Disk{diskChunks = chunks} = do
  rest :|> lastChunk <- pure chunks
  case chunkFileId lastChunk of
    Nothing -> pure (Disk rest)  -- last chunk is free, drop it
    Just fid -> do
      (freeIndex, freeSpace) <- ifind (const isEmpty) rest
      let
        lastSize = chunkSize lastChunk
        freeSize = chunkSize freeSpace
      pure case freeSize `compare` lastSize of
        EQ -> Disk $ Seq.update freeIndex lastChunk rest
        LT -> let
            lastChunk' = lastChunk { chunkSize = lastSize - freeSize }
            filledSpace = freeSpace { chunkFileId = Just fid }
          in Disk $ Seq.update freeIndex filledSpace rest :|> lastChunk'
        GT -> let
            freeSpace' = freeSpace { chunkSize = freeSize - lastSize }
          in Disk $ Seq.insertAt freeIndex lastChunk $ Seq.update freeIndex freeSpace' $ rest

fragmentDisk :: Disk -> Disk
fragmentDisk = iterateUntilNothing shuffleFragment

checksum :: Disk -> Int
checksum = go 0 0 . diskChunks
  where
    go !_ !acc Empty = acc
    go !pos !acc (chk :<| rest)
      | Nothing <- chunkFileId chk = go pos' acc rest
      | Just fid <- chunkFileId chk = go pos' (acc + fid * s) rest
      where
        sz = chunkSize chk
        pos' = pos + sz
        s = sum [pos .. pos'-1]  -- cba to wrangle the proper formula

compact :: Disk -> Disk
compact Disk{ diskChunks = chunks } = Disk $ go (Seq.filter ((>0) . chunkSize) chunks)
  where
    highestId = (length chunks + 1) `div` 2
    go Empty = Empty
      -- | curId `mod` 100 == 0, traceShow curId False = error "brek"
    go (rest :|> chk)
      = case chunkFileId chk of
        Just _
          | Just rest' <- move chk rest -> go rest' :|> chk { chunkFileId = Nothing }
        _ -> go rest :|> chk

move :: Chunk -> Seq Chunk -> Maybe (Seq Chunk)
move chk blocks = do
  ix <- Seq.findIndexL suitable blocks
  let withShrunkSpace = Seq.adjust (#chunkSize -~ chunkSize chk) ix blocks
  pure $ Seq.insertAt ix chk withShrunkSpace
  where suitable spc = isEmpty spc && chunkSize spc >= chunkSize chk

inside :: Int -> Int ->  (Seq a -> Seq a) -> Seq a -> Seq a
inside start end f xs = let
  (rest, next) = Seq.splitAt end xs
  (prev, interest) = Seq.splitAt start rest
  in prev <> f interest <> next

coalesceFree :: Seq Chunk -> Seq Chunk
coalesceFree (x :<| xs@(y :<| rest))
  | isEmpty x, isEmpty y = coalesceFree $ emptyChunk (chunkSize x + chunkSize y) :<| rest
  | otherwise = x :<| coalesceFree xs
coalesceFree xs = xs