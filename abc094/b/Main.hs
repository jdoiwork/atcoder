import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
  [n, m, x] <- readNumbers
  as <- readNumbers
  let (a, b) = span (<= x) as
      answer = minimum $ map length [a, b]
  print answer

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
