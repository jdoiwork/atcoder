type IntX = Integer
-- type IntX = Int

main :: IO ()
main = do
  n <- readLn

  print $ lucas n 2 1


lucas :: IntX -> IntX -> IntX -> IntX
lucas 0 a _ = a
lucas n a b = lucas (n - 1) b (a + b)
