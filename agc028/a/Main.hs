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
  [n1, n2] <- readNumbers
  s1 <- getLine
  s2 <- getLine
  let
    l = lcm n1 n2
    m1 = mapping l n1 s1
    m2 = mapping l n2 s2

  if isConflict m1 m2
    then writeNumbers [-1]
    else writeNumbers [l]

type IndexedString = [(IntX, Char)]

isConflict :: IndexedString -> IndexedString -> Bool
isConflict [] _ = False
isConflict _ [] = False
isConflict (x@(ix, cx):xs) (y@(iy, cy):ys)
  | ix == iy && cx /= cy = True -- Conflict!
  | ix < iy   = isConflict xs (y:ys)
  | ix > iy   = isConflict (x:xs) ys
  | otherwise = isConflict xs ys

mapping :: IntX -> IntX -> String -> IndexedString
mapping l n s = zipWith go [0..] s
  where
    go i c = (1 + ((i * l) `div` n) , c)


readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
