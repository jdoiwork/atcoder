import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (sort, minimumBy)
import Data.Ratio ((%), denominator, numerator)
import Data.Function (on)

main :: IO ()
main = do
  _n <- readNumbers
  xs <- sort . map (% 1) <$> readNumbers
  let answer = composeAll xs

  if denominator answer == 1
    then print (numerator answer)
    else print (fromRational answer :: Double)

readNumbers :: IO [Integer]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

composeAll :: [Rational] -> Rational
composeAll [] = 0
composeAll [x] = x
composeAll xs = composeAll $ compose xs

compose :: [Rational] -> [Rational]
compose [] = []
compose [x] = [x]
compose xs =
  let (a, b) = minimumPair xs
  in dropPair (a, b) xs

avg :: Rational -> Rational -> Rational
avg x y = (x + y) / 2

minimumPair :: [Rational] -> (Rational, Rational)
minimumPair xs@(_:_:_)
  = minimumBy (compare `on` (uncurry avg))
  $ zip xs (tail xs)
minimumPair _ = undefined

dropPair :: (Rational, Rational) -> [Rational] -> [Rational]
dropPair (x, y) (a:b:xs)
  | x == a && y == b = (avg x y):xs
  | otherwise = a : dropPair (x, y) (b:xs)
dropPair _ xs = xs
