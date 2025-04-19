{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM, forM_)

type IntX = Int

main :: IO ()
main = do
  [h, _w] <- readNumbers
  ss <- replicateM h T.getLine

  forM_ ss $ \s -> do
    T.putStrLn s
    T.putStrLn s

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
