{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import qualified Data.HashMap.Strict as M
import Data.List (sort)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  ss <- replicateM n T.getLine
  let table = M.fromListWith (+) [(s, 1) | s <- ss]
      (_count, answers) = M.foldrWithKey collectMax (0, []) table

  T.putStrLn $ T.unlines $ sort answers

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

collectMax :: T.Text -> Int -> (Int, [T.Text]) -> (Int, [T.Text])
collectMax s n (m, ss)
  | m < n = (n, [s])
  | m == n = (m, s : ss)
  | otherwise = (m, ss)
