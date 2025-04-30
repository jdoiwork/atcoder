{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [allCount, needCount, x] <- readNumbers

  -- hPrint stderr $ (allCount, needCount, x)
  writeNumbers $ take allCount $ (replicate needCount x) ++ (repeat (next x))

next :: IntX -> IntX
next x | x == (10^(9 :: Int)) = (10^(9 :: Int)) - 1
next x = succ x

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
