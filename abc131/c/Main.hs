{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [rMin, rMax, x1, x2] <- readNumbers

  -- hPrint stderr $ (rMin, rMax, x1, x2)
  print $ countAll rMin rMax - countAllDivs rMin rMax x1 x2

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

countDivs :: IntX -> IntX -> IntX -> IntX
countDivs rMin rMax x = (rMax `div` x) - ((rMin - 1) `div` x)

countAllDivs :: IntX -> IntX -> IntX -> IntX -> IntX
countAllDivs rMin rMax x1 x2 = count x1 + count x2 - count (lcm x1 x2)
  where
    count = countDivs rMin rMax

countAll :: IntX -> IntX -> IntX
countAll rMin rMax = rMax - rMin + 1
