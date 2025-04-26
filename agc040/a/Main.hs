{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.List (foldl1')

type IntX = Int

main :: IO ()
main = do
  ts <- T.group <$> T.getLine

  -- hPrint stderr $ (ts, map makeItem ts, foldl1' (<+>) $ map makeItem ts)
  print $ sumItem $ foldl1' (<+>) $ map makeItem ts

data Item = Item !IntX !IntX !IntX deriving (Show)

makeItem :: T.Text -> Item
makeItem t =
  let n = len t
  in case T.uncons t of
    Just ('<', _) -> Item (sumRange 0 n) 0 n
    Just ('>', _) -> Item (sumRange 0 n) n 0
    _ -> error "Invalid Input"

(<+>) :: Item -> Item -> Item
!(Item xs xl xr) <+> !(Item ys yl yr) = Item (xs + ys - (min xr yl)) xl yr

sumRange :: IntX -> IntX -> IntX
sumRange a b = (a + b) * (b - a + 1) `div` 2

len :: T.Text -> IntX
len = fromIntegral . T.length

sumItem :: Item -> IntX
sumItem (Item s _ _) = s
