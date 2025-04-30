{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

type IntX = Int

main :: IO ()
main = do
  [n, m] <- readNumbers

  table <- M.fromListWith (<>) . concat <$> replicateM m (readEdge n)

  -- hPrint stderr $ (n, m, table)
  T.putStrLn $ if canGoTo table n
    then "POSSIBLE"
    else "IMPOSSIBLE"

type Table = M.IntMap S.IntSet

canGoTo :: Table -> IntX -> Bool
canGoTo table n = not . S.null $ from1toX `S.intersection` fromNtoX
  where
    search i = M.findWithDefault [] i table
    from1toX = search 1
    fromNtoX = search n


readEdge :: IntX -> IO [(IntX, S.IntSet)]
readEdge n = do
  [a, b] <- readNumbers
  return $ case (a, b) of
    (1, _) -> [(1, [b])]
    (_, 1) -> [(1, [a])]
    _ | a == n    -> [(n, [b])]
      | b == n    -> [(n, [a])]
      | otherwise -> []

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
