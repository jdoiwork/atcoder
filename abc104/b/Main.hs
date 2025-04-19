import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.Attoparsec.Text.Lazy
import Control.Monad (void)

main :: IO ()
main = do
  s <- T.getLine

  putStrLn $ case parseOnly parser s of
    Left  _ -> "WA"
    Right _ -> "AC"

parser :: Parser ()
parser = do
  void $ char 'A'
  [_, _] <- takeWhile1 (inClass "a-z") `sepBy` char 'C'
  endOfInput
  return ()
