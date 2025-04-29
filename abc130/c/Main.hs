{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import Text.Printf (printf)

type IntX = Int

main :: IO ()
main = do
  [w, h, x, y] <- readNumbers

  printHalf (splitHalf w h x y) (isCenter w h x y)

isCenter :: IntX -> IntX -> IntX -> IntX -> Bool
isCenter w h x y = w == (x * 2) && h == (y * 2)

splitHalf :: IntX -> IntX -> IntX -> IntX -> Double
splitHalf w h _ _ = fromIntegral (w * h) / 2

printHalf :: Double -> Bool -> IO ()
printHalf x b = putStrLn $ printf "%f %d" x (fromEnum b)

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
