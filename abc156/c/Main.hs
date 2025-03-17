import Data.List (sort)

main :: IO ()
main = do
  (n:_) <- readNumbers
  xs <- sort <$> readNumbers
  let minX = head xs
      maxX = last xs
  print $ minimum $ map (`sumEnergyConsumed` xs) [minX..maxX]

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

energyConsumed :: Int -> Int -> Int
energyConsumed x p = xp * xp
  where xp = x - p

sumEnergyConsumed :: Int -> [Int] -> Int
sumEnergyConsumed p xs = sum $ map (energyConsumed p) xs
