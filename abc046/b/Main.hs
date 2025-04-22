import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [n, k] <- readNumbers

  print $ solve n k

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: IntX -> IntX -> IntX
solve n k = k * (k - 1) ^ (n - 1)
