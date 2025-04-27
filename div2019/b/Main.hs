{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [r, g, b, n] <- readNumbers

  -- hPrint stderr $ (r, g, b, n)
  print $ length $ create r g b n

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

create :: IntX -> IntX -> IntX -> IntX -> [(IntX, IntX, IntX)]
create r g b n =
  [ (rx, gx, bx)
  | rx <- takeWhile (<= n) [r * x | x <- [0..]]
  , let !na = n - rx
  , gx <- takeWhile (<= na) [g * x | x <- [0..]]
  , let !nab = na - gx
  , bx <- takeWhile (<= nab) [b * x | x <- [(nab `div` b)..]]
  , sum ([rx, gx, bx] :: [IntX]) == n
  ]
