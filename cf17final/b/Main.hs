{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.HashMap.Strict as M
import Data.List (sort)

type IntX = Int

main :: IO ()
main = do
  s <- getLine

  T.putStrLn $ case solve s of
    True -> "YES"
    False -> "NO"

solve :: String -> Bool
solve [_] = True
solve [a, b] = a /= b
solve s =
  let
    table0 = [('a', 0), ('b', 0), ('c', 0)]
    table = M.fromListWith (+) [ (c, 1) | c <- s ] `M.union` table0
    minN = minimum table
  in if minN == 0
    then False
    else solveAbc $ M.map (\x -> x - minN) table

solveAbc :: M.HashMap Char IntX -> Bool
solveAbc table =
  case sort $ M.elems table of
    [0, 1, 1] -> True
    [0, 0, 1] -> True
    [0, 0, 0] -> True
    _ -> False
