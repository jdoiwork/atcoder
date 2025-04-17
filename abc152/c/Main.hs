import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (unfoldr)

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- readNumbers

  print $ solve xs

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: [Int] -> Int
solve xs = sum $ unfoldr f (xs, head xs)
  where
    f ([], _) = Nothing
    f (x:xs, minX) = Just (fromEnum (x <= minX), (xs, min minX x))
