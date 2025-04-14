main :: IO ()
main = do
  [h, w] <- readNumbers

  print $ calc h w

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

calc :: Int -> Int -> Int
calc 1 _ = 1
calc _ 1 = 1
calc h w = h * (w `div` 2) + (w `mod` 2) * (h + 1) `div` 2
