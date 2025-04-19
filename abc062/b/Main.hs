{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)

type IntX = Int

main :: IO ()
main = do
  [h, w] <- readNumbers
  ss <- replicateM h T.getLine

  T.putStrLn $ border w
  mapM_ (T.putStrLn . quote) ss
  T.putStrLn $ border w

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

hash :: T.Text
hash = "#"

quote :: T.Text -> T.Text
quote t = hash <> t <> hash

border :: IntX -> T.Text
border n = T.replicate (fromIntegral (n + 2)) hash
