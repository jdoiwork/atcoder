{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import Control.Monad (forM_)
-- import System.IO (hPrint, stderr)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Search as V
import qualified Data.Vector.Algorithms.Intro as V
import Control.Monad.ST (runST)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- V.fromListN n <$> readNumbers
  let ys = sortBy compare xs

  -- hPrint stderr $ (n, xs, ys)
  -- hPrint stderr $ (V.tail xs, median $ V.tail xs)

  let j = medianIx1 xs

  forM_ ([0..(n-1)] :: [IntX]) $ \i -> do
    -- print i
    let
      -- (a, b) = V.splitAt i xs
      -- ab = sortBy compare $ a <> V.tail b
      si = search ys (xs V.! i)
      xs' = (if si <= j then V.tail else V.init) ys
    -- hPrint stderr $ (ab, "median =>", (i, medianIx ab, median ab))
    -- hPrint stderr $ (xs', "medianIx =>", (si, medianIx xs', median xs'))
    writeNumbers [median xs']


median :: V.Vector IntX -> IntX
median xs = xs V.! (medianIx xs)

medianIx :: V.Vector IntX -> IntX
medianIx xs = (V.length xs) `div` 2

medianIx1 :: V.Vector IntX -> IntX
medianIx1 xs = ((V.length xs) - 1) `div` 2

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs


search :: V.Vector IntX -> IntX -> Int
search xs x = runST $ do
  mvec <- V.unsafeThaw xs
  V.binarySearch mvec x

sortBy :: (IntX -> IntX -> Ordering) -> V.Vector IntX -> V.Vector IntX
sortBy cmp xs = V.create $ do
  mvec <- V.thaw xs
  V.sortBy cmp mvec
  return mvec
