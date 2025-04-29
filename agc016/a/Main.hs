{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import Data.List (nub)
-- import Debug.Trace (traceShow)

type IntX = Int

main :: IO ()
main = do
  s <- getLine
  let
    answer = minimum [solveN 0 c (T.pack s) | c <- nub s]

  writeNumbers [answer]

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs

solveN :: IntX -> Char -> T.Text -> IntX
solveN !n !c s  = solve n c $ normalize c s

solve :: IntX -> Char -> T.Text -> IntX
solve !n  _ t | T.length t == 1 = n
solve !n !c s   = solveN (n+1) c $ T.zipWith merge s $ T.tail s
  where
    merge a b
      | a == c || b == c = c
      | otherwise        = a

normalize :: Char -> T.Text -> T.Text
normalize c t =
  case T.uncons t of
    Nothing -> ""
    Just (a, cs)
      | a == c -> T.cons c $ remain
      | otherwise -> T.cons a $ normalize c cs
      where
        remain = normalize c $ T.dropWhile (==c) cs
