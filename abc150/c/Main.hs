import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
  [n] <- readNumbers
  ps <- readNumbers
  qs <- readNumbers

  print $ distance (combiOrder n ps) (combiOrder n qs)

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

combiOrder :: Int -> [Int] -> Int
combiOrder 0 _ = 1
combiOrder n (x:xs) = a + b
  where
    n' = n - 1
    a = (x - 1) * product [1..n']
    b = combiOrder n' $ map f xs
    f y = if y > x then y - 1 else y
combiOrder _ _ = error "combiOrder: invalid input"

distance :: Int -> Int -> Int
distance a b = abs (a - b)
