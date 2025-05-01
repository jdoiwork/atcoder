{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers :: IO [IntX]
  xs <- V.fromListN n . concat <$> replicateM n readNumbers :: IO (V.Vector IntX)

  -- hPrint stderr $ (n, xs)
  writeNumbers [V.foldl1' lcm xs]

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
