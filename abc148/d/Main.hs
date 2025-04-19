{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers

  case solve xs 1 (0, False) of
    (_, False) -> print (-1 :: IntX)
    (s, _) -> print s

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: [IntX] -> IntX -> (IntX, Bool) -> (IntX, Bool)
solve [] _ a = a
solve (x:xs) n (!s, a)
  | x == n    = solve xs (n + 1) (s    , True)
  | otherwise = solve xs n       (s + 1, a  )
