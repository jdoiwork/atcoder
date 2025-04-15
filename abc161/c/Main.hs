import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
  [n, k] <- readNumbers
  let x = n `mod` k
      y = distance x k

  print y

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

distance :: Int -> Int -> Int
distance a b = min a $ abs (a - b)
