{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import Data.List (sort)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- sort <$> readNumbers

  [_m] <- readNumbers
  ys <- sort <$> readNumbers

  -- hPrint stderr $ (_n, xs)
  -- hPrint stderr $ (_m, ys)

  T.putStrLn $ if solve xs ys
    then "YES"
    else "NO"

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

solve :: [IntX] -> [IntX] -> Bool
solve _ [] = True
solve [] _ = False
solve (x:xs) (y:ys)
  | x == y = solve xs ys
  | x  < y = solve xs (y:ys)
  | otherwise = False
