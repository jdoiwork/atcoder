{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.HashMap.Strict as M

type IntX = Int

main :: IO ()
main = do
  [h, w] <- readNumbers
  ss <- replicateM h getLine
  let
    bombs = createBombs h w ss
    counts = mapSurroundBombs bombs
    results = mergeTable bombs counts
    printRow = T.putStrLn . getRow w results

  -- hPrint stderr $ (h, w, ss, bombs, counts, results)
  mapM_ printRow [0..(h - 1)]

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Pos = (IntX, IntX)
type Table = M.HashMap Pos
type BombTable = Table Char
type CountTable = Table Int
type ResultTable = Table T.Text

createBombs :: IntX -> IntX -> [String] -> BombTable
createBombs h w ss = M.fromList $ filter ((=='#') . snd) $ zip ps $ concat ss
  where
    ps =  [ (y, x)
          | y <- [0..(h - 1)]
          , x <- [0..(w - 1)]
          ]

mapSurroundBombs :: BombTable -> CountTable
mapSurroundBombs bombs = M.fromListWith (+)
  [ (pos, 1)
  | origin <- M.keys bombs
  , pos <- surround origin
  ]

surround :: Pos -> [Pos]
surround (y, x) =
  [ (y + dy, x + dx)
  | dy <- [(-1)..1]
  , dx <- [(-1)..1]
  ]

mergeTable :: BombTable -> CountTable -> ResultTable
mergeTable bombs counts = M.union bombs' counts'
  where
    bombs' = M.map T.singleton bombs
    counts' = M.map (T.pack . show) counts

getRow :: IntX -> ResultTable -> IntX -> T.Text
getRow w results y = T.concat $ map find $ [(y, x) | x <- [0..(w - 1)]]
  where
    find pos = M.findWithDefault "0" pos results
