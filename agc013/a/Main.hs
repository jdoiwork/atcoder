{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.List (group)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers
  let
    cs = filter (/= EQ) $ zipWith compare xs $ tail xs
    answer = length $ split $ group cs

  -- hPrint stderr $ (_n, xs, cs, group cs)
  -- hPrint stderr $ split $ group cs
  print $ max 1 answer

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

split :: [[Ordering]] -> [[Ordering]]
split (a:[GT]:b:cs) = a : split (b:cs)
split (a:[LT]:b:cs) = a : split (b:cs)
split (c:cs) = c : split cs
split [] = []
