import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [cities, roads] <- readNumbers
  xs <- replicateM roads readNumbers
  let table = V.replicate (cities + 1) 0 :: V.Vector IntX
      query = [(a, 1) | x <- xs, a <- x]
      answer = V.tail $ V.accum (+) table query

  V.mapM_ print answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
