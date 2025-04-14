main :: IO ()
main = do
  [_n] <- readNumbers
  [k] <- readNumbers
  xs <- readNumbers
  let robotA = makeRobot 0
      robotB = makeRobot k
  print $ sum $ map (\x -> min (robotA x) (robotB x)) xs


readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

makeRobot :: Int -> (Int -> Int)
makeRobot n x = double $ distance n x

distance :: Int -> Int -> Int
distance n x = abs (n - x)

double :: Int -> Int
double x = x * 2
