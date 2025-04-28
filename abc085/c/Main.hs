{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM, guard)
-- import System.IO (hPrint, stderr)
import Text.Printf (printf)
-- import Debug.Trace (traceShow)


type IntX = Int

main :: IO ()
main = do
  [n, y] <- readNumbers
  let
    goal = y `div` 1000
    wallet = [10, 5, 1]

  case solve goal n wallet of
    [] -> printResult failedResult
    (answer:_) -> do
      -- hPrint stderr $ (sum $ zipWith (*) wallet answer, goal == (sum $ zipWith (*) wallet answer))
      printResult answer

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

solve :: IntX -> IntX -> [IntX] -> [[IntX]]
solve goal limit [a, b, c] =
  [ [x1, x2, x3]
  | x1 <- reverseCounts limit
  , let x1a = x1 * a
  , x1a <= goal
  , x2 <- reverseCounts (limit - x1)
  , let x2b = x2 * b
  , x2b <= goal - x1a
  , x3 <- [limit - x1 - x2]
  , let x3c = x3 * c
  , limit == x1 + x2 + x3
  , goal == x1a + x2b + x3c
  ]
solve _ _ _ = error "[solve] Invalid Input"


reverseCounts :: IntX -> [IntX]
reverseCounts x = [x,(x-1)..0]

failedResult :: [IntX]
failedResult = [-1, -1, -1]

printResult :: [IntX] -> IO ()
printResult [a, b, c] = putStrLn $ printf "%d %d %d" a b c
printResult xs = error $ "Invalid result: " ++ show xs
