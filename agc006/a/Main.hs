{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  s <- T.getLine
  t <- T.getLine

  -- hPrint stderr $ (_n, s, t, merge s t)
  print $ T.length $ merge s t

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

merge :: T.Text -> T.Text -> T.Text
merge s t = go (T.length t)
  where
    go n
      | T.isSuffixOf (T.take n t) s = s <> T.drop n t
      | otherwise = go (n - 1)
