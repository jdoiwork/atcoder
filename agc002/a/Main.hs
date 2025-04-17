import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Printf

main :: IO ()
main = do
  [a, b] <- readNumbers

  putStrLn $ solve a b


readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

count :: Int -> Int -> Int
count a b
  | a == b    = 0
  | otherwise = b - a + 1

solve :: Int -> Int -> String
solve a b
  | a > 0 = "Positive"
  | a <= 0 && 0 <= b = "Zero"
  | b < 0 = if odd (count a b) then "Negative" else "Positive"
  | otherwise = printf "Error: %d %d" a b
