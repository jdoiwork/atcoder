import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import Data.List (sort)

type IntX = Int

main :: IO ()
main = do
  [n, _l] <- readNumbers
  ss <- replicateM n T.getLine

  T.putStrLn $ T.concat $ sort ss

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
