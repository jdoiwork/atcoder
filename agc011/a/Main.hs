{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as V

import qualified Data.Sequence as Q
import Data.Sequence (Seq(..))


import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [n, bus, limit] <- readNumbers
  xs <- sortBy compare . V.fromListN n . concat <$> replicateM n readNumbers
  let y = V.foldl' (debounce bus limit) ([], 0) xs

  -- hPrint stderr $ (n, xs, y)
  writeNumbers [snd y]

type BusCapa = IntX
type Limit = IntX
type BusCount = IntX
type Accum = (Q.Seq IntX, BusCount)

debounce :: BusCapa -> Limit -> Accum -> IntX -> Accum
debounce capa limit (q@(l :<| _), !count) time
  | ((Q.length q < capa) && ((time - l) <= limit))
      = (q :|> time, count)
debounce _ _ (_, count) time = ([time], count + 1)
  -- # キューがEmpty
  --  - カウントアップ
  --  - time を新規キューにする
  -- # キューに追加可能
  --  ## ( キューのサイズがキャパ未満
  --        AND
  --       (time - キューの最初) <= limit
  --     )
  -- # それ以外
  --  - カウントアップ
  --  - timeを新規キューにする


readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs

sortBy :: (IntX -> IntX -> Ordering) -> V.Vector IntX -> V.Vector IntX
sortBy cmp xs = V.create $ do
  mvec <- V.unsafeThaw xs
  V.sortBy cmp mvec
  return mvec
