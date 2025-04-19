import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [t8, t10] <- readNumbers

  let t8s = costRange 8 t8
      t10s = costRange 10 t10
      answer = findCommonMin t8s t10s

  print $ case answer of
    Just x  -> x
    Nothing -> -1

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

inverseTax :: IntX -> IntX -> IntX
inverseTax rate t =  (t * 100) `div` rate

calcTax :: IntX -> IntX -> IntX
calcTax rate x = (x * rate) `div` 100

costRange :: IntX -> IntX -> [IntX]
costRange rate t = [x | x <- ts, calcTax rate x == t]
  where
    ts = [inverseTax rate (t - 1) .. inverseTax rate (t + 1)]

findCommonMin :: [IntX] -> [IntX] -> Maybe IntX
findCommonMin [] _ = Nothing
findCommonMin _ [] = Nothing
findCommonMin (x:xs) (y:ys)
  | x == y = Just x
  | x < y  = findCommonMin xs (y:ys)
  | otherwise = findCommonMin (x:xs) ys
