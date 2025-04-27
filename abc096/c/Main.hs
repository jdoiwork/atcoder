{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.HashSet as S

type IntX = Int

main :: IO ()
main = do
  [h, w] <- readNumbers
  ss <- replicateM h getLine
  let
    table = createBombs h w ss
    answer = all (isAdjacent table) $ S.toList table

  -- hPrint stderr $ (h, w, ss, table, answer)
  T.putStrLn $ if answer then "Yes" else "No"

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Pos = (IntX, IntX)
type Table = S.HashSet Pos
type BombTable = Table

surround :: Pos -> [Pos]
surround (y, x) =
  [ (y + dy, x + dx)
  | dy <- [(-1)..1]
  , dx <- [(-1)..1]
  , (abs dy + abs dx) == 1
  ]

isAdjacent :: BombTable -> Pos -> Bool
isAdjacent table p = any (`S.member` table) $ surround p

createBombs :: IntX -> IntX -> [String] -> BombTable
createBombs h w ss = S.fromList $ map fst $ filter ((=='#') . snd) $ zip ps $ concat ss
  where
    ps =  [ (y, x)
          | y <- [0..(h - 1)]
          , x <- [0..(w - 1)]
          ]
