{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V

import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import Data.Ord (comparing)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- sortBy (comparing snd) . V.fromListN n . map toTask <$> replicateM n readNumbers

  -- hPrint stderr $ (n, xs)
  T.putStrLn $ case runTasks xs of
    Just _ -> "Yes"
    Nothing -> "No"

runTasks :: V.Vector Task -> Maybe IntX
runTasks ts = V.foldM go 0 ts
  where
    go (x) (time, dead)
      | x + time > dead = Nothing
      | otherwise       = Just $! x + time

type LeadTime = IntX
type Deadline = IntX
type Task = (LeadTime, Deadline)

toTask :: [IntX] -> Task
toTask [a, b] = (a, b)
toTask _ = error "Invalid input"


readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

sortBy :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
sortBy cmp xs = V.create $ do
  mvec <- V.thaw xs
  V.sortBy cmp mvec
  return mvec
