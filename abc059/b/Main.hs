type IntX = Integer

main :: IO ()
main = do
  a <- readLn :: IO IntX
  b <- readLn :: IO IntX

  putStrLn $ case compare a b of
    LT -> "LESS"
    EQ -> "EQUAL"
    GT -> "GREATER"
