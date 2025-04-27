import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.IntMap.Strict as M

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers
  let table = createTable xs

  -- hPrint stderr $ (_n, xs, table)
  print $ maximum $ M.elems table

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

createTable :: [IntX] -> M.IntMap IntX
createTable xs = M.fromListWith (+)
  [ (x + d, 1 :: IntX)
  | x <- xs
  , d <- [(-1)..1]
  ]
