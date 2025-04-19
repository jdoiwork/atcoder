{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.List (foldl')
import Control.Lens
type IntX = Int

data Solver = Solver
  { _lastX        :: !IntX
  , _currentCount :: !IntX
  , _maxCount     :: !IntX
  } deriving (Show)

makeLenses ''Solver


main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers
  let answer = updateCell $ foldl' solve seed xs

  print $ answer ^. maxCount

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine


seed :: Solver
seed = Solver 0 0 0

solve :: Solver -> IntX -> Solver
solve solver x
  | solver ^. lastX < x = solver & updateCell & lastX .~ x & currentCount .~ 0
  | otherwise           = solver              & lastX .~ x & currentCount +~ 1

updateCell :: Solver -> Solver
updateCell s
  | s^.currentCount > s^.maxCount
      = s & maxCount .~ (s^.currentCount)
  | otherwise = s
