import Data.Bits (shiftL, (.&.))

main :: IO ()
main = do
  n <- readLn

  print $ head $ filter ((>0) . (n .&.)) bits

bits :: [Int]
bits = reverse [shiftL 1 x | x <- [0..6]]
