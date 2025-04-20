{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Set as S
import Control.Lens (both, over, (&))
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  s <- T.getLine

  hPrint stderr $ (s, T.foldl' f (S.empty, S.empty) s)
  let result = T.foldl' f (S.empty, S.empty) s
  T.putStrLn $ case result & over both S.size of
    (1, _) -> "No"
    (_, 1) -> "No"
    _      -> "Yes"


type DirState = (S.Set Char, S.Set Char)

f :: DirState -> Char -> DirState
f (s, t) 'N' = (S.insert 'N' s, t)
f (s, t) 'S' = (S.insert 'S' s, t)
f (s, t) 'E' = (s, S.insert 'E' t)
f (s, t) 'W' = (s, S.insert 'W' t)
f ds _ = ds

