import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Integer

main :: IO ()
main = do
  [a, b, _c, k] <- readNumbers

  print $ f k a b

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

f :: IntX -> IntX -> IntX -> IntX
f k a b
  | even k    = a - b
  | otherwise = b - a
