main :: IO ()
main = interact $ show . sumRange . pred . read

sumRange :: Int -> Int
sumRange n = (1 + n) * n `div` 2
