{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main2 where

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import Control.Monad (replicateM)
import System.IO (hPrint, stderr)
import Data.List (sort)
import qualified Data.Vector.Unboxed as V
import qualified Data.Heap as H
import Data.Vector.Algorithms.Search as V
import Control.Monad.ST (runST)
import Control.Lens
import Debug.Trace (traceShow)

type IntX = Int

type Vec = V.Vector IntX

data Range = Range
  { _values    :: Vec
  , _distances :: Vec
  , _maxDistance :: !IntX
  , _maxIndex :: !Int
  } deriving (Show)

makeLenses ''Range

instance Ord Range where
  compare r1 r2 = compare (r1 ^. maxDistance) (r2 ^. maxDistance)

instance Eq Range where
  r1 == r2 = (r1 ^. maxDistance) == (r2 ^. maxDistance)


makeRange :: Vec -> Range
makeRange xs = Range xs ds md mi
  where
    ds = V.cons 0 $ V.zipWith (flip (-)) xs $ V.tail xs
    (mi, md) =
      if valuesDistance >= snd imaxDis
      then (search xs valuesCenter, valuesDistance)
      else imaxDis
    valuesDistance = (V.last xs - V.head xs) `div` 2
    valuesCenter = (V.head xs + V.last xs) `div` 2
    imaxDis = V.ifoldl' go (0, V.head ds) ds
      where
        go (ai, ax) bi bx
          | ax < bx   = (bi, bx)
          | otherwise = (ai, ax)

sumRange :: Range -> IntX
sumRange r = r ^. distances & V.sum

main :: IO ()
main = do
  [n, m] <- readNumbers
  xs <- V.fromListN m . sort <$> readNumbers

  -- hPrint stderr $ (n, m)
  hPrint stderr $ (xs)
  if n >= m
  then print 0
  else do
    let
      ds = V.cons 0 $ V.zipWith (flip (-)) xs $ V.tail xs
      r0 = makeRange xs
      answers = solve n 1 (H.singleton r0)

    -- hPrint stderr $ (ds, V.maxIndex ds)
    -- let y = (V.head xs + V.last xs) `div` 2
    -- hPrint stderr $ (y, search xs y, xs V.! (search xs y), V.splitAt (search xs y) xs)
    hPrint stderr $ (r0)
    hPrint stderr $ (answers)
    print $ sum $ map sumRange answers

solve :: IntX -> IntX -> H.MaxHeap Range -> [Range]
solve !limit !now rs
  | limit == now = H.toList rs
  | otherwise    =
      case traceShow ("solve", now, rs) $ H.view rs of
        Nothing -> []
        Just (r, rs') ->
          let (r1, r2) = splitRange r
          in solve limit (now + 1) (rs' <> H.fromList [r1, r2])
    -- 最も長い部分を探す
    -- 最も長い部分を分割する
    -- 再帰

splitRange :: Range -> (Range, Range)
splitRange r = (makeRange vs1, makeRange vs2)
  where
    (vs1, vs2) = V.splitAt (r ^. maxIndex) (r ^. values)

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs

search :: V.Vector IntX -> IntX -> Int
search xs x = runST $ do
  mvec <- V.unsafeThaw xs
  V.binarySearch mvec x

