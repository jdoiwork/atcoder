import Data.List (sortBy, foldl')
import Data.Ord (comparing, Down (Down))

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- sortBy (comparing Down) <$> readNumbers
  let y = foldl' (\(a, b) x -> (b + x, a)) (0, 0) xs
  print $ distance y

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

distance :: (Int, Int) -> Int
distance (x, y) = abs (x - y)
