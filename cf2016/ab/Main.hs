import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.HashMap.Strict as M

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  rabbits <- readNumbers
  let table = M.fromListWith (+) $ zipWith pair [1..] rabbits

  -- hPrint stderr $ (_n, rabbits, table)
  print $ M.size $ M.filter (>1) table

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Rabbit = IntX
type Pair = (Rabbit, Rabbit)
type Point = IntX

sort :: Rabbit -> Rabbit -> Pair
sort a b
  | a > b     = (b, a)
  | otherwise = (a, b)

withPoint :: a -> (a, Point)
withPoint x = (x, 1)

pair :: Rabbit -> Rabbit -> (Pair, Point)
pair a b = withPoint $ sort a b
