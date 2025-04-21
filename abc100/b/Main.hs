import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM, forM_)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [d, n] <- readNumbers

  -- hPrint stderr $ (n, d)
  let n' = if n == 100 then 101 else n
  print $ solve d n'

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

solve :: IntX -> IntX -> IntX
solve d n = n * (10 ^ (d * 2))
