import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  ts <- V.fromList . (0:) <$> readNumbers :: IO (V.Vector IntX)
  [m] <- readNumbers
  pxs <- replicateM m readNumbers

  let x = V.sum ts
      answers = map (\[i, v] -> x - (ts V.! i) + v) pxs

  mapM_ print answers

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
