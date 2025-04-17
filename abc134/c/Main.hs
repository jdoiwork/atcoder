{-# LANGUAGE RecordWildCards #-}

import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Function (on)

main :: IO ()
main = do
  n <- readLn
  xs <- replicateM n readLn :: IO [Int]
  let best = findBest xs
      otherBests = map (otherBest best) xs

  mapM_ print otherBests

data Best = Best
  { count :: !Int
  , best1 :: !Int
  , best2 :: !Int
  } deriving (Show, Eq)

findBest :: [Int] -> Best
findBest (x:xs) = foldl' go (Best 1 x 0) xs
  where
    go :: Best -> Int -> Best
    go best@(Best{..}) x
      | x >  best1 = Best 1 x best1
      | x == best1 = best { count = count + 1 }
      | otherwise  = best { best2 = max best2 x }

otherBest :: Best -> Int -> Int
otherBest (Best{..}) x
  | x == best1 && count > 1 = best1
  | x == best1              = best2
  | otherwise               = best1
