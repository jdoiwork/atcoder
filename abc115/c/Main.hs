{-# OPTIONS_GHC -O2 #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
-- import Data.List (sortBy)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as VA


type IntX = Int

main :: IO ()
main = do
  [n, k] <- readNumbers
  xs <- sortVector . V.fromList . concat <$> replicateM n readNumbers
  let y = V.minimum $ V.zipWith (-) xs $ V.drop (k - 1) xs

  -- hPrint stderr $ (n, k, xs, y)
  print y

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

sortVector :: V.Vector IntX -> V.Vector IntX
sortVector vec = V.create $ do
  mvec <- V.thaw vec
  VA.sortBy (flip compare) mvec
  return mvec
