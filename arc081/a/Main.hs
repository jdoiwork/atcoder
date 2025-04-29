{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.IntMap as M
import Control.Lens
import Data.List (sortBy)
import Data.Ord (comparing)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers

  let
    table = M.fromListWith (+) [(x, 1 :: IntX) | x <- xs]
    sticks = table
      & M.filter (>1)
      & M.toList
      & sortBy (flip (comparing fst))

  -- hPrint stderr $ (n, xs, table, sticks)
  writeNumbers $ case sticks of
    ((a, n):_) | n >= 4 -> [a * a]
    (a:b:_) -> [fst a * fst b]
    _       -> [0]


readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
