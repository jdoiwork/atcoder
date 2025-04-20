{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import System.IO (hPrint, stderr)
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Data.Sequence (Seq(..))

type IntX = Int

main :: IO ()
main = do
  s <- T.getLine

  print $ fst $ findMaxPattern' "" s (0, Q.empty)

findMaxPattern' :: T.Text -> T.Text -> (IntX, Q.Seq T.Text) -> (IntX, Q.Seq T.Text)
findMaxPattern' _ "" n = n
findMaxPattern' last remain (n, q)
  | last == remain && T.length remain == 1 = (n, joinLast remain q)
  | last == T.take 1 remain = findMaxPattern' (T.take 2 remain) (T.drop 2 remain) ((n + 1), q :|> T.take 2 remain)
  | otherwise = findMaxPattern' (T.take 1 remain) (T.drop 1 remain) ((n + 1), q :|> T.take 1 remain)
  where
    joinLast :: T.Text -> Q.Seq T.Text -> Q.Seq T.Text
    joinLast last Q.Empty = Q.singleton last
    joinLast last (q :|> x) = q :|> (x <> last)
