{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import Control.Monad (forM_)
-- import System.IO (hPrint, stderr)

import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- ((V.replicate (n + 2) 0) V.//) . zip [1..] <$> readNumbers
  let
    ds = V.zipWith distance xs $ V.tail xs
    sumD = V.sum ds

  -- hPrint stderr $ (n, xs)
  -- hPrint stderr $ (n, ds)
  forM_ ([1..n] :: [IntX]) $ \i -> do
    writeNumbers [skipAt xs ds sumD i]

type Vec = V.Vector IntX

skipAt :: Vec -> Vec -> IntX -> IntX -> IntX
skipAt xs ds !sumD i = sumD - xSumD + newDistance
  where
    xSumD = V.sum $ V.slice (i - 1) 2 ds
    newDistance = distance (xs V.! (i - 1)) (xs V.! (i + 1))

distance :: IntX -> IntX -> IntX
distance a b = abs $ a - b

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
