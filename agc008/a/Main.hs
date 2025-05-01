{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [now, goal] <- readNumbers

  -- hPrint stderr $ (now, goal, plan now goal)
  writeNumbers [plan now goal]


plan :: IntX -> IntX -> IntX
plan now goal =
  let
    disA = distance now goal
    disB = distance (- now) goal
    planA = min disA disB
  in case () of
    _ | now == goal   -> 0 -- [EQ-1]
      | now == - goal -> 1 -- [EQ-2]
      | now - planA   == goal -> planA + 2 -- [N-2]
      | - planA - now == goal -> planA + 1 -- [I-2]
      | planA - now   == goal -> planA + 1 -- [I-1]
      | otherwise             -> planA     -- [N-1]
      -- EQ-1. すでにゴール a = 0, b = 0
      -- EQ-2. 反転したらゴール a = 0, b = 1
      -- N-1. 上昇するほうが近い a = x, b = 0
      -- N-2. 下降するほうが近い a = x, b = 2: 4 => 3: -4
      -- I-1. 反転してから上昇するほうが近い a = x, b = 1: 3 => -2: -3, -2
      -- I-2. 反転してから下降するほうが近い a = x, b = 1: 3 => -4: 4, - 4


distance :: IntX -> IntX -> IntX
distance x y = abs $ x - y

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
