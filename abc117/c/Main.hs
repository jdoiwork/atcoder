{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as V

type IntX = Int

main :: IO ()
main = do
  [n, m] <- readNumbers
  xs <- sortBy compare . V.fromListN m <$> readNumbers

  let
    ds = sortBy (flip compare) $ V.zipWith (flip (-)) xs $ V.tail xs
    answer = V.sum $ V.drop (n - 1) ds

  writeNumbers [answer]

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs

sortBy :: (IntX -> IntX -> Ordering) -> V.Vector IntX -> V.Vector IntX
sortBy cmp xs = V.create $ do
  mvec <- V.thaw xs
  V.sortBy cmp mvec
  return mvec
