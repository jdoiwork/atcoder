{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.IntSet as S
import Data.Char (ord)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  s <- T.getLine

  -- hPrint stderr $ (n, s, [countSame s x | x <- [1..(n - 1)]])
  print $ maximum [countSame s x | x <- [1..(n - 1)]]

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

countSame :: T.Text -> IntX -> IntX
countSame t n = S.size $ S.intersection sa sb
  where
    (a, b) = T.splitAt (fromIntegral n) t
    sa = text2set a
    sb = text2set b

text2set :: T.Text -> S.IntSet
text2set t = S.fromList [ord $ T.head c | c <- T.chunksOf 1 t]
