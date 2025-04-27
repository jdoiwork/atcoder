{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import Control.Monad (replicateM)
import qualified Data.IntSet as S
import Data.Functor.Identity (runIdentity)
import Data.List (foldl')

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- concat <$> replicateM n readNumbers

  print $ S.size $ foldl' flipKey S.empty xs

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

flipKey :: S.IntSet -> Int -> S.IntSet
flipKey s x = runIdentity $ S.alterF (return . not) x s
