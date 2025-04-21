import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [n, a, b] <- readNumbers

  -- hPrint stderr $ (n, a, b, solve n a b)
  print $ solve n a b

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

moveToCenter :: IntX -> IntX -> IntX -> IntX
moveToCenter _ a b = abs (a - b) `div` 2

moveToRight :: IntX -> IntX -> IntX -> IntX
moveToRight n a b = (n - b + 1) + (moveToCenter n (a + (n - b)) n)

moveToLeft :: IntX -> IntX -> IntX -> IntX
moveToLeft n a b = (a) + moveToCenter n 1 (b - a)

solve :: IntX -> IntX -> IntX -> IntX
solve n a b
  | even (a - b) = moveToCenter n a b
  | otherwise    = min (moveToRight n a b) (moveToLeft n a b)
