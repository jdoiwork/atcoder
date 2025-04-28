{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

type IntX = Int

main :: IO ()
main = do
  [n, m] <- readNumbers

  print $ solve n m

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

solve :: IntX -> IntX -> IntX
solve 1 1 = 1
solve 1 b = solve b 1
solve a 1 = a - 2
solve a b = (a - 2) * (b - 2)
