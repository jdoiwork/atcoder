main :: IO ()
main = do
  (n:_) <- readNumbers
  ns <- readNumbers
  putStrLn $ toResult $ isMonotonicallyIncreasing ns


readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

isMonotonicallyIncreasing :: [Int] -> Bool
isMonotonicallyIncreasing cs = all id $ zipWith (<) cs (tail cs)

toResult :: Bool -> String
toResult True = "Yes"
toResult False = "No"
