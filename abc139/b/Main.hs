main :: IO ()
main = do
  (a:b:_) <- readNumbers
  print $ solve 0 a b


readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

solve :: Int -> Int -> Int -> Int
solve ans a b
  | b <= 1 = ans
  | otherwise = solve (ans + 1) a (b - a + 1)
