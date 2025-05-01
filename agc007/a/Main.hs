{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

import Control.Monad.Extra (concatForM)
-- import System.IO (hPrint, stderr)

import qualified Data.HashSet as S

type IntX = Int

main :: IO ()
main = do
  [h, w] <- readNumbers
  table <- S.fromList <$> concatForM [1..h] readRow

  -- hPrint stderr $ (h, w, table, goToGoal table (1, 1) (h, w))

  T.putStrLn $ if null (goToGoal table (1, 1) (h, w))
    then "Possible"
    else "Impossible"

type Pos = (Int, Int)
type Table = S.HashSet Pos

goToGoal :: Table -> Pos -> Pos -> Table
goToGoal table pos goal =
  let
    newTable = S.delete pos table
  in if pos == goal
    then newTable
    else case nextSteps newTable pos of
      [next] -> goToGoal newTable next goal
      _      -> newTable

nextSteps :: Table -> Pos -> [Pos]
nextSteps table (a, b) =
  [ p
  | p <- [(a + 1, b), (a, b + 1)]
  , S.member p table
  ]

readRow :: IntX -> IO [Pos]
readRow y = do
  s <- getLine
  return [(y, x) | (x, c) <- withIndex s, c == '#']

withIndex :: [a] -> [(IntX, a)]
withIndex = zip [1..]

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
