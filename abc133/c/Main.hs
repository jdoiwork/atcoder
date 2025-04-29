{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
  [l, r] <- readNumbers

  -- hPrint stderr $ (l, r)
  writeNumbers $ if r - l >= 2019
    then [0]
    else [solve l r]

solve :: IntX -> IntX -> IntX
solve l r = min0 2019
  [ mod2019 (mod2019 i * mod2019 j)
  | i <- [l..r]
  , j <- [(i+1)..r]
  ]

min0 :: IntX -> [IntX] -> IntX
min0 a [ ]    = a
min0 _ (0:_)  = 0
min0 a (x:xs) = min0 (min a x) xs

mod2019 :: IntX -> IntX
mod2019 x = x `mod` 2019

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
