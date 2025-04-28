{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n, k] <- readNumbers
  xs <- V.fromListN n <$> readNumbers
  let
    minI = V.minIndex xs
    l = minI
    r = V.length xs - (minI + 1)

  -- hPrint stderr $ (n, k, xs, V.minIndex xs, V.length xs - (minI + 1))
  print $ (l + r) </> k

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

(</>) :: Int -> Int -> Int
a </> b = d + x
  where
    (d, m) = a `divMod` (b - 1)
    x = if m == 0 then 0 else 1
