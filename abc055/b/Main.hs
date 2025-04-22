import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers

  print $ foldr (*%) 1 [1..n]

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

m :: IntX
m = 10 ^ 9 + 7

(*%) :: IntX -> IntX -> IntX
a *% b = (a * b) `mod` m
