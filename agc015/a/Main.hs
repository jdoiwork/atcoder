{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [n, a, b] <- readNumbers

  -- hPrint stderr $ (n, a, b)
  print $ solve n a b

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

solve :: IntX -> IntX -> IntX -> IntX
solve n a b
  | a > b            = 0
  | n > 1            = (n - 2) * (b - a) + 1
  | n == 1 && a == b = 1
  | otherwise        = 0
