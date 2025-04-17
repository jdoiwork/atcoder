import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
  [n, blue, red] <- readNumbers

  print $ solve n blue red

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: Int -> Int -> Int -> Int
solve n blue red = blues + remain
  where
    (x, y) = n `divMod` (blue + red)
    blues = x * blue
    remain = min y blue
