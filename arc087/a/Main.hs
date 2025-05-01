{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.IntMap as M

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers
  let table = M.fromListWith (+) [(x, 1) | x <- xs]

  -- hPrint stderr $ (n, xs, table, map countToBeRemoved $ M.toList table)
  writeNumbers [sum $ map countToBeRemoved $ M.toList table]

countToBeRemoved :: (IntX, IntX) -> IntX
countToBeRemoved (x, c)
  | c < x = c
  | c > x = c - x
  | otherwise = 0


readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
