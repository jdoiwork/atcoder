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
  [_n, p] <- readNumbers
  xs <- readNumbers
  let vs = V.accum (+) [0, 0] [(x `mod` 2, 1) | x <- xs] :: V.Vector IntX
  -- hPrint stderr $ (n, p, xs, vs)
  writeNumbers [solve p vs]

solve :: IntX -> V.Vector IntX -> IntX
solve 1 vs = (countEvens (vs V.! 0)) * (countOdds (vs V.! 1))
solve 0 vs = (countEvens (vs V.! 0)) * (2 ^ (vs V.! 1) - (countOdds (vs V.! 1)))
solve _ _ = error "Invalid input"

countOdds :: IntX -> IntX
countOdds x = (2 ^ x) `div` 2

countEvens :: IntX -> IntX
countEvens x = (2 ^ x)

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
