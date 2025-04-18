{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Prelude hiding (flip)

main :: IO ()
main = do
  s <- T.getLine

  print $ minimum $ map (count s) ["01", "10"]

flip :: T.Text -> T.Text -> T.Text
flip s t = T.zipWith changed s $ T.cycle t

count :: T.Text -> T.Text -> Int
count s t = fromIntegral . T.count (T.singleton '1') $ flip s t

changed :: Char -> Char -> Char
changed a b = if a == b then ' ' else '1'
