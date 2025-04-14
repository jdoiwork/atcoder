main :: IO ()
main = do
  x <- read . concat . words <$> getLine
  putStrLn $ if isSquareNumber x then "Yes" else "No"

isSquareNumber :: Int -> Bool
isSquareNumber n = (isqrt n ^ (2 :: Int)) == n
  where
    isqrt :: Int -> Int
    isqrt = floor . (sqrt :: Double -> Double) . fromIntegral
