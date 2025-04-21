import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- readNumbers

  print $ sum [ x - 1 | x <- xs ]

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
