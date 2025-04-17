{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [nPlayer, kPoint, qRound] <- readNumbers
  xs <- replicateM qRound readLn :: IO [Int]
  let players = V.replicate nPlayer (kPoint - qRound)
      y = V.accum (+) players $ [(i - 1, 1) | i <- xs]

  V.mapM_ (T.putStrLn . toAlive) y

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

toAlive :: Int -> T.Text
toAlive n = if n > 0 then "Yes" else "No"
