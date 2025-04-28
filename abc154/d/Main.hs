{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n, k] <- readNumbers
  xs <- V.map (+1) . V.fromListN n <$> readNumbers
  let
    sum0 = V.sum $ V.take k xs
    queries = V.zipWith (-) (V.drop k xs) xs
    values = V.scanl' (+) sum0 queries
    answer = (fromIntegral $ V.maximum values) / (2 :: Double)

  print answer

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
