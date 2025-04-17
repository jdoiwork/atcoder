import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (unfoldr)
import Data.Bits (shiftR)

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers

  print $ minimum $ map countBits xs

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

bits :: Int -> [Int]
bits x = unfoldr f x
  where
    f 0 = Nothing
    f n = Just (fromEnum $ odd n, shiftR n 1)

countBits :: Int -> Int
countBits = length . takeWhile (== 0) . bits
