{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers

  -- hPrint stderr $ (n, xs, incStep 0 xs)
  T.putStrLn $ incStep 0 xs

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

incStep :: IntX -> [IntX] -> T.Text
incStep _ [] = "Yes"
incStep n (x:xs)
  | n <= x = incStep (max n (x - 1)) xs
  | otherwise = "No"
