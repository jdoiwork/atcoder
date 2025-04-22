{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.Vector.Unboxed as V
import Data.List (permutations)
import qualified Control.Monad.State.Strict as S
import qualified Data.HashMap.Strict as M
import Control.Lens

type IntX = Int

type Path = [Int]
type Pos = (Double, Double)
type Memo = M.HashMap Path Double

data AppContext
  = AppContext
  { _memo :: !Memo
  , _posTable :: V.Vector Pos
  } deriving (Show)

makeLenses ''AppContext

type App = S.State AppContext

main :: IO ()
main = do
  [n] <- readNumbers
  ps <- V.fromListN n <$> replicateM n readPos

  -- hPrint stderr $ (n, ps, permutations [0..2])
  let
    paths = permutations [0..(n - 1)]
    ctx = AppContext M.empty ps
    xs = ctx & S.evalState (findPathList paths)
    answer = sum xs / (fromIntegral $ length xs)
  -- hPrint stderr $ (xs, answer)
  print answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

readPos :: IO Pos
readPos = do
  [a, b] <- map fromIntegral <$> readNumbers
  return (a, b)

findPath :: [IntX] -> App Double
-- 2 path
findPath [a, b] = do
  target <- uses memo (M.!? [a, b])
  case target of
    Just x -> return x
    Nothing -> do
      x <- calcPath a b
      memo %= (M.insert [a, b] x)
      return x

-- 3+ path
findPath path@(a:b:ps) = do
  target <- uses memo (M.!? path)
  case target of
    Just x -> return x
    Nothing -> do
      x <- calcPath a b
      y <- findPath (b:ps)
      memo %= (M.insert path (x + y))
      return $ x + y

findPath path = error $ "Invalid path: " ++ show path

findPathList :: [[IntX]] -> App [Double]
findPathList ps = mapM findPath ps

calcPath :: IntX -> IntX -> App Double
calcPath a b = do
  (ax, ay) <- getPos a
  (bx, by) <- getPos b
  return $ sqrt $ (ax - bx) ** 2 + (ay - by) ** 2


getPos :: IntX -> App Pos
getPos a = uses posTable (V.! a)
