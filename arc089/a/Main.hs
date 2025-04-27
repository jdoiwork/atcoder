{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.Foldable (foldlM)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- replicateM n readNumbers
  let start = [0, 0, 0]

  -- hPrint stderr $ (n, xs, foldlM (\_ x -> uncurry movable x) () $ zip ([0,0,0]:xs) xs)
  T.putStrLn $ case foldlM movable' () $ zip (start:xs) xs of
    Just _ -> "Yes"
    Nothing -> "No"

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

movable' :: () -> ([IntX], [IntX]) -> Maybe ()
movable' _ = uncurry movable

movable :: [IntX] -> [IntX] -> Maybe ()
movable [t1, x1, y1] [t2, x2, y2] =
  let
    td = t2 - t1
    d = manDis (x1, y1) (x2, y2)
  in case compare td d of
    LT -> Nothing
    EQ -> Just ()
    GT  | even (t2 - t1 - d) -> Just ()
        | otherwise -> Nothing

movable _ _ = Nothing

manDis :: (IntX, IntX) -> (IntX, IntX) -> IntX
manDis (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
