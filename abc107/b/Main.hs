{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [h, _w] <- readNumbers
  ts <- replicateM h T.getLine

  -- hPrint stderr $ (h, _w, ts)
  T.putStrLn $ T.unlines $ solve ts

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: [T.Text] -> [T.Text]
solve = (o . o)
  where o = T.transpose . filter (T.isInfixOf "#")
