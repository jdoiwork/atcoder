{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import System.IO (hPrint, stderr)
import Data.List (sortBy)
import Data.Ord (comparing)

type IntX = Int

data Score = Score
  { city :: !T.Text
  , score :: !IntX
  , index :: !IntX
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  [n] <- readNumbers
  ss <- map makeScore . zip [1..] <$> replicateM n readScore
  let sortedScores = sortBy (comparing city <> flip (comparing score)) ss
  hPrint stderr $ (n, ss)
  mapM_ (print . index) sortedScores

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

readScore :: IO (T.Text, IntX)
readScore = do
  [city, score] <- T.words <$> T.getLine
  return (city, read (T.unpack score) :: IntX)

makeScore :: (IntX, (T.Text, IntX)) -> Score
makeScore (i, (city, score)) = Score
  { city = city
  , score = score
  , index = i
  }
