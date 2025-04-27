{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.Vector.Unboxed as V
import Data.List (foldl')

type IntX = Int

main :: IO ()
main = do
  xs <- V.fromListN 9 . concat <$> replicateM 3 readNumbers
  let [sa, sb, sc] = [sum' $ getRow xs offset | offset <- [0..2]]

  -- hPrint stderr $ (xs, getRow xs 0, getRow xs 1, getRow xs 2)
  T.putStrLn $ if (sa == sb) && (sb == sc)
    then "Yes"
    else "No"

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

pos :: (Int, Int) -> Int
pos (y, x) = y * 3 + x

getRow :: V.Vector IntX -> IntX -> [IntX]
getRow v offset =
  [ v V.! pos ((i + offset) `mod` 3, i)
  | i <- [0..2]
  ]

sum' :: [IntX] -> IntX
sum' = foldl' (+) 0
