type IntX = Integer

main :: IO ()
main = do
  h <- readLn :: IO IntX

  print $ fight h

fight :: IntX -> IntX
fight 1 = 1
fight n
  | even n    = 1 + fight (n `div` 2) * 2
  | otherwise = 1 + fight ((n - 1) `div` 2) * 2
