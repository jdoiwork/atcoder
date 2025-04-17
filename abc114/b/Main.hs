main :: IO ()
main = do
  s <- getLine
  let ns = map (distance . read) $ take3s s :: [Int]

  print $ minimum ns


take3s :: [a] -> [[a]]
take3s [] = []
take3s xs = take 3 xs : take3s (tail xs)

distance :: Int -> Int
distance n = abs $ n - 753
