{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import qualified Data.Text as TS

import Control.Monad (replicateM)
import Data.List (sort)
import qualified Data.HashMap.Strict as M


type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  ss <- replicateM n getLine

  let table = M.fromListWith (+) [ ((TS.pack $ sort s), 1) | s <- ss ]

  writeNumbers [sum $ map countPatterns $ M.elems table]


countPatterns :: IntX -> IntX
countPatterns l = (len * (len + 1)) `div` 2
  where
    len = l - 1

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
