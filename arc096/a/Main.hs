import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [a, b, c, x, y] <- readNumbers
  let n = min x y
      m = (n `div` 2)
      n2 = m * 2
      x' = x - n2
      y' = y - n2
      p2 = (pair2 (a, b, c, x, y)) * m
      p1 = pair1 (a, b, c, x', y')
      answer = p1 + p2

  print answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type PizzaSet = (IntX, IntX, IntX, IntX, IntX)

pair2 :: PizzaSet -> IntX
pair2 (a, b, c, _x, _y) = min ab cc
  where
    ab = (a + b) * 2
    cc = c * 4

pair1 :: PizzaSet -> IntX
pair1 (a, b, c, x, 0) = (min a (c * 2)) * x
pair1 (a, b, c, 0, y) = (min b (c * 2)) * y
pair1 (a, b, c, x, y) = min ab cc
  where
    ab = a * x + b * y
    cc = (c * 2) * (max x y)
-- pair1 (a, b, c, x, y) = min ab cc
--   where
--     ab = a * x + b * y
--     cc = (c * 2) * (max x y)
