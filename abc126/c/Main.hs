{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.Ratio (Ratio, (%))

type IntX = Integer

main :: IO ()
main = do
  [n, k] <- readNumbers
  let
    xs = [ (1 % 2) ^ (calcIndex m k) | m <- [1..n]] :: [Ratio IntX]
    answer = fromRational $ sum xs * (1 % n) :: Double

  -- hPrint stderr $ (n, k, sum xs * (1 % n))
  print answer

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

calcIndex :: IntX -> IntX -> IntX
calcIndex n a
  | n >= a = 0
  | otherwise = ceiling $ logBase 2 (double a / double n)
  where
    double :: IntX -> Double
    double = fromIntegral
