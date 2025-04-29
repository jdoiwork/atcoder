{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.Sequence as Q
import Data.Sequence (Seq(..))
import qualified Data.Vector.Unboxed as V
import Data.Foldable (toList)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- V.fromListN n <$> readNumbers

  let y = V.foldl' push (False, Q.empty) xs

  -- hPrint stderr $ (n, xs, y)
  writeNumbers $ case y of
    (False, q) -> toList q
    ( True, q) -> toList $ Q.reverse q

type Queue = Q.Seq IntX
push :: (Bool, Queue) -> IntX -> (Bool, Queue)
push (False, q) x = ( True, q :|> x)
push ( True, q) x = (False, x :<| q)


readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
