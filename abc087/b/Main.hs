import Control.Monad (replicateM)

main :: IO ()
main = do
  [a, b, c, x] <- replicateM 4 readLn :: IO [Int]
  let coins = [500, 100, 50]
      wallet = zip coins [a, b, c]

  print $ solve wallet x

solve :: [(Int, Int)] -> Int -> Int
solve [] 0 = 1
solve [] _ = 0
solve ((coin, count):rest) x
  | x < 0 = 0
  | otherwise = sum [solve rest (x - coin * i) | i <- [0..count]]
