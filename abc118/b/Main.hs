import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import Control.Monad (replicateM)

main :: IO ()
main = do
  [n, m] <- readNumbers
  xs <- replicateM n (tail <$> readNumbers)
  let table = V.replicate (m + 1) 0 :: V.Vector Int
      y = V.accum (+) table [withCount x | x <- concat xs]
      z = V.length $ V.filter (==n) y
  print z

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

withCount :: Int -> (Int, Int)
withCount n = (n, 1)
