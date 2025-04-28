{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.IntMap.Strict as M
import Data.List (sort)

type IntX = Int

main :: IO ()
main = do
  [_n, k] <- readNumbers
  xs <- readNumbers
  let
    table = M.fromListWith (+) [ (x, 1) | x <- xs ]
    s = M.size table
    m = max 0 $ s - k
    answer = sum $ take m $ sort $ M.elems table :: IntX

  -- hPrint stderr $ (_n, k, xs, table, sort y, m)
  print answer

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
