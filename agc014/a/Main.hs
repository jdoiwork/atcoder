{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad.State.Strict

main :: IO ()
main = do
  [a, b, c] <- readNumbers

  let initialState = ExchangeState S.empty 0
      result = evalState (solve (a, b, c)) initialState
  print result

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Cookies = (Int, Int, Int)

exchange :: Cookies -> Cookies
exchange (a, b, c) = (half b + half c, half a + half c, half a + half b)

half :: Int -> Int
half n = n `div` 2

data ExchangeState = ExchangeState
  { history :: S.Set Cookies
  , count :: Int
  } deriving (Show)

type Exchange = State ExchangeState

solve :: Cookies -> Exchange Int
solve cookies = do
  (ExchangeState{..}) <- get
  if S.member cookies history
  then return (-1)
  else do
    if existsOdd cookies
    then return count
    else do
      put $ ExchangeState (S.insert cookies history) (count + 1)
      solve $ exchange cookies

existsOdd :: Cookies -> Bool
existsOdd (a, b, c) = odd a || odd b || odd c
