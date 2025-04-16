import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad (replicateM)

main :: IO ()
main = do
  [n] <- readNumbers
  [d, k] <- readNumbers
  xs <- replicateM n readLn

  print $ k + sum (map (`eatChocolate` d) xs)

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

eatChocolate :: Int -> Int -> Int
eatChocolate x d = (d - 1 + x) `div` x
