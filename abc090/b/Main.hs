import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
  [a, b] <- readNumbers

  print $ length $ inRange a b palindromics

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

palindromics :: [Int]
palindromics =
    [ a * 10001 + b * 1010 + c * 100
    | a <- [1..9]
    , b <- [0..9]
    , c <- [0..9]
    ]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b = takeWhile (<=b) . dropWhile (<a)
