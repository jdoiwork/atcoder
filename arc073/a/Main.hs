{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.List (foldl')

type IntX = Int

main :: IO ()
main = do
  [_n, t] <- readNumbers
  xs <- readNumbers

  print $ fst $ foldl' (shower t) (0, -t) xs

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

type Accum = (IntX, IntX)

shower :: IntX -> Accum -> IntX -> Accum
shower t !(s, lastT) now
  | lastT + t <= now = (s + t, now)
  | otherwise        = ((s + t - w), now)
  where w = lastT + t - now
