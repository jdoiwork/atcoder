import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers

  -- hPrint stderr $ (_n, xs, solve xs)
  print $ solve xs

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: [IntX] -> IntX
solve [] = 0
solve (x:xs)
  | even x     = 3 ^ (length xs) + (2 * (solve xs))
  | otherwise  = 2 * 3 ^ (length xs) + (solve xs)
