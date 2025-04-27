{-# LANGUAGE BangPatterns #-}

import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.List (foldl')

type IntX = Int

main :: IO ()
main = do
  n <- readLn :: IO IntX
  xs <- replicateM n readLn :: IO [IntX]

  -- hPrint stderr $ (n, xs, foldl' merge (101, 0, 0) xs)
  print $ calcMax $ foldl' merge (101, 0, 0) xs

type Accum = (IntX, IntX, IntX)
merge :: Accum -> IntX -> Accum
merge !(minX, sumX, sum10) x
  | isMod10 x = (minX, sumX, sum10 + x)
  | otherwise = (min minX x, sumX + x, sum10)

calcMax :: Accum -> IntX
calcMax !(minX, sumX, sum10)
  | sumX == 0    = 0
  | isMod10 sumX = sumX - minX + sum10
  | otherwise    = sumX + sum10

isMod10 :: IntX -> Bool
isMod10 x = x `mod` 10 == 0
