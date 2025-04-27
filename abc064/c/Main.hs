{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.IntSet as S
import Data.List (foldl')
import Text.Printf (printf)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- map ((`div` 400) . (min 3200)) <$> readNumbers
  let (s, m) = foldl' merge (S.empty, 0) xs

  -- hPrint stderr $ (_n, xs, foldl' merge (S.empty, 0) xs)
  putStrLn $ printf "%d %d" (max 1 (S.size s)) (S.size s + m)

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

merge :: (S.IntSet, IntX) -> IntX -> (S.IntSet, IntX)
merge !(s, n) 8 = (s, n + 1)
merge !(s, n) x = (S.insert x s, n)
