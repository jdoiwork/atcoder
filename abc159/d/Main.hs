{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.IntMap.Strict as M
import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- V.fromListN n <$> readNumbers

  let
    table = M.fromListWith (+) $ [ (x, 1) | x <- V.toList xs ]
    !allBalls = sumBalls table

  -- hPrint stderr $ (_n, xs, table, combi 4 3, selectBalls table $ head xs)
  V.mapM_ (print . selectBalls allBalls table) xs

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

combi :: IntX -> IntX -> IntX
combi a b = y `div` x
  where
    y = product $ take b [a,(a-1)..]
    x = product $ take b [b,(b-1)..]

type Table = M.IntMap IntX

selectBalls :: IntX -> Table -> IntX -> IntX
selectBalls allBalls table x =
  let
    n = table M.! x
    a = combi n 2
    b = combi (n - 1) 2
  in allBalls - a + b

sumBalls :: Table -> IntX
sumBalls table = M.foldl' accumCombi 0 table

accumCombi :: IntX -> IntX -> IntX
accumCombi accum n = accum + combi n 2
