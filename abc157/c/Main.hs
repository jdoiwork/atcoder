{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import Control.Lens


type IntX = Int

main :: IO ()
main = do
  [n, m] <- readNumbers
  table <- M.fromListWith S.union <$> replicateM m readConstraints

  -- hPrint stderr $ (n, m, table)
  print $ case solve n table of
    []    -> (-1)
    (x:_) -> x

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

readConstraints :: IO (IntX, S.IntSet)
readConstraints = do
  [s, c] <- readNumbers
  return $ (s, S.singleton c)

type Table = M.IntMap S.IntSet

solve :: IntX -> Table -> [IntX]
solve n table = [1..n]
  & map (getPossibleValues table n)
  & combinations
  & map digit10


getPossibleValues ::Table -> IntX -> IntX -> [IntX]
getPossibleValues table n i =
  case table M.!? i of
    Nothing
      | is1stDitit -> [1..9]
      | otherwise  -> [0..9] -- default values
    Just s
      | is1stDitit    -> S.toList $ S.filter (/= 0) s
      | S.size s == 1 -> S.toList s
      | otherwise     -> []
  where is1stDitit = n /= 1 && i == 1

combinations :: [[a]] -> [[a]]
combinations [] = [[]]
combinations (xs:xss) = [x:ys | x <- xs, ys <- combinations xss]

digit10 :: [IntX] -> IntX
digit10 xs = foldl' go 0 xs
  where
    go a b = a * 10 + b
