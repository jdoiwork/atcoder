import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List (sort)

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- V.fromList . sort <$> readNumbers
  let index = n `div` 2
      prev  = index - 1
  print $ (xs V.! index) - (xs V.! prev)

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
