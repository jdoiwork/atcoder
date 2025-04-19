import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.List (unfoldr)

type IntX = Int

main :: IO ()
main = do
  [n, a, b] <- readNumbers

  print $ sum [x | x <- [1..n], let s = sumDigits x, a <= s && s <= b]

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

sumDigits :: IntX -> IntX
sumDigits n = sum $ unfoldr go n
  where
    go 0 = Nothing
    go x = Just (x `mod` 10, x `div` 10)
