import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.List (sort)

type IntX = Int

main :: IO ()
main = do
  xs <- readNumbers

  print $ solve xs 0

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: [IntX] -> IntX -> IntX
solve xs a
  | allSame xs = a
  | otherwise  = solve (f xs) (a + 1)
  where
    f = add . flat . sort

flat :: [IntX] -> [IntX]
flat xs = [x - a | x <- xs] where a = minimum xs

add :: [IntX] -> [IntX]
add [0, 0, 1] = [1, 1, 1]
add [a, b, c] = [a + 2, b, c]
add _ = error "[add] Invalid input"

allSame :: [IntX] -> Bool
allSame [a, b, c] = (a == b) && (b == c)
allSame _ = error "[allSame] Invalid input"
