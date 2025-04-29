{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [s, c] <- readNumbers

  -- hPrint stderr $ (s, c)
  writeNumbers [solveSsc s c]

solveSsc :: IntX -> IntX -> IntX
solveSsc s c = sscs + solveOnlyC c'
  where
    sscs = min s (c `div` 2)
    c' = c - 2 * sscs

solveOnlyC :: IntX -> IntX
solveOnlyC c = c `div` 4

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
