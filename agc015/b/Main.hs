{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  s <- getLine
  let n = length s

  -- hPrint stderr $ (s, n)
  writeNumbers [sum $ zipWith (count n) s [1..]]

count :: IntX -> Char -> IntX -> IntX
count n 'U' x = 2 * len 1 (x - 1) +     len (x+1) n
count n 'D' x =     len 1 (x - 1) + 2 * len (x+1) n
count _ _ _ = error "Invalid input"

len :: IntX -> IntX -> IntX
len a b = b - a + 1

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
