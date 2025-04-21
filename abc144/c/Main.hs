import Data.List (minimumBy)
import Data.Ord (comparing)
type IntX = Int

main :: IO ()
main = do
  n <- readLn

  print $ countMoving $ minAbsDiff n

type Couple = (IntX, IntX)

countMoving :: Couple -> IntX
countMoving (i, j) = i + j - 2

isqrt :: IntX -> IntX
isqrt x = floor $ sqrt (fromIntegral x :: Double)

minAbsDiff :: IntX -> Couple
minAbsDiff n = minimumBy (comparing distance) xs
  where
    xs =
      [ (x, n `div` x)
      | x <- [1..(isqrt n)]
      , n `mod` x == 0
      ]

distance :: Couple -> Int
distance (a, b) = abs (a - b)
