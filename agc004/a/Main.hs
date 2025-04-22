import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.List (sort)

type IntX = Int

main :: IO ()
main = do
  xs <- readNumbers

  -- hPrint stderr $ (xs)
  print $ solve $ sort xs

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: [IntX] -> IntX
solve xs@[a, b, _]
  | any even xs = 0
  | otherwise   = a * b
solve _ = error "Invalid input"
