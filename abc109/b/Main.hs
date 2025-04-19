{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import qualified Data.Set as S

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  (s:ss) <- replicateM n T.getLine

  T.putStrLn $ shiritori ((T.last s), S.singleton s) ss

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Memo = S.Set T.Text

shiritori :: (Char, Memo) -> [T.Text] -> T.Text
shiritori _ [] = "Yes"
shiritori (c, memo) (x:xs)
  | S.member x memo = "No"
  | canJoin c x = shiritori (T.last x, S.insert x memo) xs
  | otherwise = "No"

canJoin :: Char -> T.Text -> Bool
canJoin c x = c == T.head x
