import Control.Monad
import Data.List
import Data.Function (on)

main :: IO ()
main = do
  (n:m:_) <- readNumbers
  shops <- sortByCost . map toDrinkItem <$> replicateM n readNumbers
  print $ calc 0 m shops

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

sortByCost :: [(Int, Int)] -> [(Int, Int)]
sortByCost = sortBy (compare `on` fst)

toDrinkItem :: [Int] -> (Int, Int)
toDrinkItem (a:b:_) = (a, b)
toDrinkItem _ = error "Invalid input"

calc :: Int -> Int -> [(Int, Int)] -> Int
calc cost 0 _ = cost
calc cost m ((a, b): shops) = calc (cost + a * b') (m - b') shops
  where
    b' = min m b
calc _ _ _ = error "Invalid input"
