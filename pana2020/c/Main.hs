{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

-- import System.IO (hPrint, stderr)

type IntX = Integer

main :: IO ()
main = do
  [a, b, c] <- readNumbers

  -- hPrint stderr $ (a, b, c)
  -- 4 * a * b < (c - a - b) ^ 2
  let d = c - a - b

  T.putStrLn $ if 0 < d && (4 * a * b) < (d ^ (2 :: IntX))
    then "Yes"
    else "No"

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
