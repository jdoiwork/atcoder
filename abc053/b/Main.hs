{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = T.interact $
  T.pack . show . T.length .
  fst . T.breakOnEnd "Z" .
  snd . T.breakOn "A"
