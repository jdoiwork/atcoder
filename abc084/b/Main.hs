import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import Control.Monad (replicateM_, void)

main :: IO ()
main = do
  [a, b] <- readNumbers
  s <- T.getLine

  -- print $ (a, b, s)
  putStrLn $ case P.parse (parser a b) s of
    P.Done _ _ -> "Yes"
    _          -> "No"


readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

parser :: Int -> Int -> P.Parser String
parser a b = do
  replicateM_ a P.digit
  void $ P.char '-'
  replicateM_ b P.digit
  return "Yes"
