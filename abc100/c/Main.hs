import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers

  print $ sum $ map (countDiv2s 0) xs

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

countDiv2s :: IntX -> IntX -> IntX
countDiv2s n x
  | even x    = countDiv2s (n + 1) (x `div` 2)
  | otherwise = n
