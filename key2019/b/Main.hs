{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import Control.Lens


main :: IO ()
main = do
  s <- getLine

  T.putStrLn $ case solve ("keyence", s) of
    ("", _) -> "YES"
    _       -> "NO"

dropSameHead :: (String, String) -> (String, String)
dropSameHead ((x:xs), (y:ys)) | x == y = dropSameHead (xs, ys)
dropSameHead (xs, ys) = (xs, ys) & over each reverse

solve :: (String, String) -> (String, String)
solve = dropSameHead . dropSameHead
