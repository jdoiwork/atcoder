{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T


main :: IO ()
main = do
  s <- T.getLine
  t <- T.getLine

  T.putStrLn $ if t `T.isInfixOf` (s <> s)
    then "Yes"
    else "No"
