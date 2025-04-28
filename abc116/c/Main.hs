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
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..))

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- V.fromListN n <$> readNumbers

  -- hPrint stderr $ (n, xs)
  writeNumbers $ [solve 0 [xs]]

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs

type Row = V.Vector IntX
type Queue = Q.Seq Row

solve :: IntX -> Queue -> IntX
solve n Q.Empty = n
solve n (xs :<| q) =
  let
    minX = V.minimum xs
    ys = V.map (\a -> a - minX) xs
    zs = filter (not . V.any (==0)) $ V.groupBy (\a b -> (a /= 0) == (b /= 0)) ys
  in solve (n + minX) (q <> Q.fromList zs)
