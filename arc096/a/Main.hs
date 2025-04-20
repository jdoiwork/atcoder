import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [a, b, c, x, y] <- readNumbers
  let n = min x y
      x' = x - n
      y' = y - n
      p2 = (pair2 (a, b, c, x, y)) * n
      p1 = pair1 (a, b, c, x', y')
      answer = p1 + p2

  print answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type PizzaSet = (IntX, IntX, IntX, IntX, IntX)

pair2 :: PizzaSet -> IntX
pair2 (a, b, c, _x, _y) = min ab cc
  where
    ab = (a + b)
    cc = c * 2

pair1 :: PizzaSet -> IntX
pair1 (a, b, c, x, y) = sx + sy
  where
    sx = (min a (c * 2)) * x
    sy = (min b (c * 2)) * y
