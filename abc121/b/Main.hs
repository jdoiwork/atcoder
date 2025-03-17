import Control.Monad

main :: IO ()
main = do
  (n:m:c:_) <- readNumbers
  bs <- (c:) <$> readNumbers
  as <- map (1:) <$> replicateM n readNumbers
  print $ length $ filter (>0) $ map (dot bs) as
  return ()

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

dot :: [Int] -> [Int] -> Int
dot bs as = sum $ zipWith (*) bs as
