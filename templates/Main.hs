{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers

  hPrint stderr $ (n)

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
