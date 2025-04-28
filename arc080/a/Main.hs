{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import Prelude hiding (round)
import qualified Data.IntMap.Strict as M

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers
  let
    table = M.fromListWith (+) $ [(round x, 1) | x <- xs]
    answer = solve1 ((table *! 1), (table *! 2), (table *! 4))

  -- hPrint stderr $ (_n, xs, map round xs, table)
  T.putStrLn $ if answer then "Yes" else "No"

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

round :: IntX -> IntX
round x
  | x %? 4    = 4
  | x %? 2    = 2
  | otherwise = 1

(%?) :: IntX -> IntX -> Bool
a %? b = a `mod` b == 0

type Deck = (IntX, IntX, IntX)

solve1 :: Deck -> Bool
solve1 (x1, x2, x4)
  | x1 == x4 + 1 = solve2 (0, x2, 0) 1
  | x1 <= x4     = solve2 (0, x2, x4 - x1) 4
  | otherwise    = False

solve2 :: Deck -> IntX -> Bool
solve2 (_,  0,  _) _ = True
solve2 (_,  _,  _) 1 = False
solve2 (_, x2, x4) 4
  | x2 > 1           = solve2 (0, (x2 `mod` 2), x4) 4
solve2 (_,  1,  _) _ = True
solve2 _           _ = False

type Table = M.IntMap IntX

(*!) :: Table -> IntX -> IntX
table *! i = M.findWithDefault 0 i table
