main :: IO ()
main = do
  (n:_) <- readNumbers
  -- x = n / 1.08
  -- x = n / (108 / 100)
  -- x = n * (100 / 108)

  let x = genka n
      ans = filter ((==n) . tax) [x, x + 1]
  case ans of
    [] -> putStrLn ":("
    (x:_) -> print x

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

genka :: Int -> Int
genka n = n * 100 `div` 108

tax :: Int -> Int
tax x = x * 108 `div` 100
