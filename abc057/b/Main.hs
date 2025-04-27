import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.Ord (comparing)
import Data.List (minimumBy)

type IntX = Int

main :: IO ()
main = do
  [n, m]   <- readNumbers
  students <- map list2pos <$> replicateM n readNumbers
  points   <- map list2pos <$> replicateM m readNumbers

  -- hPrint stderr $ (n, m, students, points)
  mapM_ (print . nearestPoint points) students

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Pos = (IntX, IntX)

list2pos :: [IntX] -> Pos
list2pos [a, b] = (a, b)
list2pos x = error $ "Invalid input: " ++ show x

manDis :: Pos -> Pos -> IntX
manDis (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

nearestPoint :: [Pos] -> Pos -> IntX
nearestPoint ps x = fst $ minimumBy (distanceAsc <> indexAsc) $ zip [1..] ds
  where
    distanceAsc = comparing snd
    indexAsc = comparing fst
    ds = map (manDis x) ps
