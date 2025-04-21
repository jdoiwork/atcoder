import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import qualified Data.HashMap.Strict as M

type IntX = Int

main :: IO ()
main = do
  blues <- readTable
  reds <- fmap negate <$> readTable

  let result = M.unionWith (+) blues reds
  print $ maximum $ 0 : (M.elems result)


readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Table = M.HashMap T.Text IntX

makeTable :: [T.Text] -> Table
makeTable ts = M.fromListWith (+) [(t, 1) | t <- ts]

readTable :: IO Table
readTable = do
  [n] <- readNumbers
  makeTable <$> replicateM n T.getLine
