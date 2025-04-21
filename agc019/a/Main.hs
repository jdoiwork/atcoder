import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%))
import qualified Data.HashMap.Strict as M
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..))
import Text.Printf (printf)

type IntX = Int

main :: IO ()
main = do
  qs <- readNumbers
  [n4] <- map (*4) <$> readNumbers
  let cps = sortBy (comparing (uncurry (%))) $ zip qs ([1, 2, 4, 8] :: [IntX])
      (bestCost, bestSize) = head $ filter ((<= n4) . snd) cps
      batch = n4 `div` bestSize
      originSize = batch * bestSize
      originCost = batch * bestCost
      memo = M.singleton originSize originCost
      tasks = Q.singleton originSize
  -- hPrint stderr $ (qs, n4, cps)
  -- hPrint stderr $ (memo, tasks, n4, bestSize)
  -- hPrint stderr $ (solve cps memo tasks n4)
  print $ solve cps memo tasks n4


readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Cost = IntX
type Size = IntX
type Memo = M.HashMap IntX IntX
type Queue = Q.Seq IntX

solve :: [(Size, Cost)] -> Memo -> Queue -> Size -> Cost
solve _ memo Q.Empty target = getCost memo target
solve cps memo (t :<| ts) target = solve cps memo' ts' target
  where
    baseCost = getCost memo t
    isMinCost size cost = case memo M.!? size of
      Nothing -> True
      Just x -> cost < x
    nexts = M.fromList $
      [ (size + t, cost + baseCost)
      | (cost, size) <- cps
      , let cost' = cost + baseCost
      , let size' = size + t
      , size' <= target
      , isMinCost size' cost'
      ]
    memo' = M.unionWith min memo nexts
    ts' = (Q.fromList $ M.keys nexts) <> ts

getCost :: Memo -> IntX -> IntX
getCost memo target = case memo M.!? target of
      Just x -> x
      Nothing -> error $ printf "Nothing: %d, %s" target (show memo)
