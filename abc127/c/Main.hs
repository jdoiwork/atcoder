import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad (replicateM)
import Data.List (foldl')

main :: IO ()
main = do
  [n, m] <- readNumbers
  xs <- replicateM m readNumbers
  let y = foldl' passGate (1, n) xs

  print $ countIds y

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type IdRange = (Int, Int)
type Gate = [Int]

passGate :: IdRange -> Gate -> IdRange
passGate (l, r) [x, y] = (max l x, min r y)
passGate _ _ = error "Invalid input"

countIds :: IdRange -> Int
countIds (l, r) = max 0 $ r - l + 1
