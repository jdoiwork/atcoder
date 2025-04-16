main :: IO ()
main = do
  n <- readLn :: IO Int

  print $ head [p | p <- primes, p >= n]

primes :: [Int]
primes = sieve (2:[3,5..])
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve []     = []
