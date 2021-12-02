module Main where

import Data.Char (chr, isSpace)
import Data.Function ((&))
import Data.Word (Word8)
import GHC.Conc (numCapabilities)
import Streamly.Data.Array.Foreign (Array)
import qualified Streamly.Data.Array.Foreign as Array
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Fold.Tee (Tee (Tee))
import qualified Streamly.Data.Unicode.Stream as Stream
import qualified Streamly.Internal.Data.Fold.Tee as Tee
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Prelude as Stream
import WordCount (Counts (Counts), count)

-- Count bytes (wc -c)
wcb :: String -> IO Int
wcb file =
  File.toBytes file -- SerialIT IO Word8
    & Stream.fold Fold.length -- IO Int

-- Count lines (wc -l)

-- ASCII character 10 is a newline.
countl :: Int -> Word8 -> Int
countl n ch = if ch == 10 then n + 1 else n

-- The fold accepts a stream of `Word8` and returns a line count (`Int`).
nlines :: Monad m => Fold m Word8 Int
nlines = Fold.foldl' countl 0

wcl :: String -> IO Int
wcl file =
  File.toBytes file -- SerialIT IO Word8
    & Stream.fold nlines -- IO Int

-- Count words (wc -w)
countw :: (Int, Bool) -> Word8 -> (Int, Bool)
countw (n, wasSpace) ch
  | isSpace $ chr $ fromIntegral ch = (n, True)
  | wasSpace = (n + 1, False)
  | otherwise = (n, False)

-- The fold accepts a stream of `Word8` and returns a word count (`Int`).
nwords :: Monad m => Fold m Word8 Int
nwords = fst <$> Fold.foldl' countw (0, True)

wcw :: String -> IO Int
wcw file =
  File.toBytes file -- SerialIT IO Word8
    & Stream.fold nwords -- IO Int

-- Counting Bytes, Words and Lines together
-- The fold accepts a stream of `Word8` and returns the three counts.
countAll :: Fold IO Word8 (Int, Int, Int)
countAll = Tee.toFold $ (,,) <$> Tee Fold.length <*> Tee nlines <*> Tee nwords

wc :: String -> IO (Int, Int, Int)
wc file =
  File.toBytes file -- SerialIT IO Word8
    & Stream.fold countAll -- IO (Int, Int, Int)

-- Concurrent Word Counting
countArray :: Array Word8 -> IO Counts
countArray arr =
  Stream.unfold Array.read arr -- SerialT IO Word8
    & Stream.decodeLatin1 -- SerialT IO Char
    & Stream.foldl' count (Counts 0 0 0 True) -- IO Counts

{-
When combining the counts in two contiguous chunks, we need to check whether the first element of the next chunk is a whitespace character in order to determine if the same word continues in the next chunk or whether the chunk starts with a new word. The partialCounts function adds a Bool flag to Counts returned by countArray to indicate whether the first character in the chunk is a space.
-}
partialCounts :: Array Word8 -> IO (Bool, Counts)
partialCounts arr = do
  let r = Array.getIndex arr 0
  case r of
    Just x -> do
      counts <- countArray arr
      pure (isSpace (chr (fromIntegral x)), counts)
    Nothing -> pure (False, Counts 0 0 0 True)

addCounts :: (Bool, Counts) -> (Bool, Counts) -> (Bool, Counts)
addCounts (sp1, Counts l1 w1 c1 ws1) (sp2, Counts l2 w2 c2 ws2) =
  let wcount =
        if not ws1 && not sp2 -- No space between two chunks.
          then w1 + w2 - 1
          else w1 + w2
   in (sp1, Counts (l1 + l2) wcount (c1 + c2) ws2)

wc' :: String -> IO (Bool, Counts)
wc' file =
  do
    Stream.unfold File.readChunks file -- AheadT IO (Array Word8)
    & Stream.mapM partialCounts -- AheadT IO (Bool, Counts)
    & Stream.maxThreads numCapabilities -- AheadT IO (Bool, Counts)
    & Stream.foldl' addCounts (False, Counts 0 0 0 True) -- IO (Bool, Counts)

main :: IO ()
main = undefined
