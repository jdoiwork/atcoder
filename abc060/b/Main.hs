{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.IntSet as S

type IntX = Int

main :: IO ()
main = do
  [a, b, c] <- readNumbers
  let s = S.fromList $ take b [(a * i) `mod` b| i <- [0..]]

  -- hPrint stderr $ (a, b, c, s, S.member c s)
  T.putStrLn $ if S.member c s then "YES" else "NO"

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
