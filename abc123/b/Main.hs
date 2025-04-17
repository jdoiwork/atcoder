import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = do
  foods <- sortByPriority <$> replicateM 5 readLn :: IO [Int]

  print $ sum $ zipWith ($) makes foods

priority :: Int -> Int
priority n = if x == 0 then 10 else x
  where x = n `mod` 10

sortByPriority :: [Int] -> [Int]
sortByPriority = sortBy (compare `on` priority)

makeAndWait :: Int -> Int
makeAndWait n
  | n `mod` 10 == 0 = n
  | otherwise       = ((n `div` 10) + 1) * 10

makeOnly :: Int -> Int
makeOnly n = n

makes :: [(Int -> Int)]
makes = makeOnly : repeat makeAndWait
