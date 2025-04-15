import Data.List (unfoldr)
import qualified Data.IntMap.Strict as M

main :: IO ()
main = do
  n <- readLn :: IO Int
  let xs = zip [1..] $ collatzSeq n
  print $ solve M.empty xs

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSeq :: Int -> [Int]
collatzSeq = unfoldr (\x -> Just (x, collatz x))

type Index = Int
type Value = Int

solve :: M.IntMap Int -> [(Index, Value)] -> Int
solve m ((i, v):xs) =
  if M.member v m
    then i
    else solve (M.insert v i m) xs
solve _ [] = error "No solution found"
