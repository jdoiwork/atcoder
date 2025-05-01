{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import System.IO (hPrint, stderr)

import Data.List (foldl')
import Control.Lens

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
    fa x = x + planA
    fb x = - x
  in minimum
    [ cost
    | (f, cost) <-
        [ ([], 0)
        , ([fb], 1)
        , ([fa], planA)
        , ([fb, fa], planA + 1)
        , ([fa, fb], planA + 1)
        , ([fb, fa, fb], planA + 2)
        ]
    , let x = foldl' (&) now f
    , x == goal
    ]
    -- []
    -- [b]
    -- [a]
    -- [b a]
    -- [a b]
    -- [b a b]

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
