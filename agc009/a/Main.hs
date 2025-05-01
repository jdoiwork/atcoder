{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- V.replicateM n (toItem <$> readNumbers)

  let y = V.foldr countMod 0 xs
  -- hPrint stderr $ (n, xs)
  writeNumbers [y]

type Item = (IntX, IntX)
toItem :: [IntX] -> Item
toItem [a, b] = (a, b)
toItem _ = error "Invalid input"

countMod :: Item -> IntX -> IntX
countMod (x, scale) count =
  let
    m = (x + count) `mod` scale
  in if m == 0
    then count
    else count + (scale - m)

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
