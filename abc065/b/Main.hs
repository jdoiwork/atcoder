{-# LANGUAGE BangPatterns #-}

import qualified Data.Vector.Unboxed as V
import qualified Data.IntSet as S

type IntX = Int

main :: IO ()
main = do
  n <- readLn :: IO IntX
  xs <- V.cons 0 <$> V.replicateM n readLn :: IO (V.Vector IntX)

  print $ solve xs S.empty 0 1

type Table = V.Vector IntX
type Memo = S.IntSet
type Count = IntX
type Index = IntX

solve :: Table -> Memo -> Count -> Index -> Count
solve _ _ !count 2 = count
solve table memo !count i
  | i `S.member` memo = -1
  | otherwise = solve table (S.insert i memo) (count + 1) (table V.! i)
