import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Printf (printf)
import Data.Complex

main :: IO ()
main = do
  [x1, y1, x2, y2] <- readNumbers

  let a = (x1 :+ y1)
      b = (x2 :+ y2)
      c = rotate a b
      d = rotate b c

  putStrLn $ formatNumbers c d

rotate :: Complex Double -> Complex Double -> Complex Double
rotate z1 z2 = z3
  where
    -- z3 - z2 = (z1 - z2) * i
    z3 = (z1 - z2) * (0 :+ (-1)) + z2

readNumbers :: IO [Double]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

formatNumbers :: Complex Double -> Complex Double -> String
formatNumbers (x3 :+ y3) (x4 :+ y4) = printf "%.0f %.0f %.0f %.0f" x3 y3 x4 y4
