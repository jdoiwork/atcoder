import Control.Monad
import qualified Data.HashMap.Strict as M
import qualified Data.IntSet as I

main :: IO ()
main = do
  bingo <- readBingo
  (n:_) <- readNumbers
  balls <- concat <$> replicateM n readNumbers
  let bingoState = createBingoState bingo
  print $ solve balls bingoState

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

readBingo :: IO Bingo
readBingo = createBingo <$> replicateM 3 readNumbers

type Pos = (Int, Int)
type Bingo = M.HashMap Pos Int

createBingo :: [[Int]] -> Bingo
createBingo bingo
  = M.fromList
  $ concatMap (\(i, row) -> map (\(j, v) -> ((i, j), v)) (withIndex row)) (withIndex bingo)

type BingoSet = [Pos]
bingoRow :: Int -> BingoSet
bingoRow i = [(i, j) | j <- [0..2]]

bingoCol :: Int -> BingoSet
bingoCol j = [(i, j) | i <- [0..2]]

bingoDiag1 :: BingoSet
bingoDiag1 = [(i, i) | i <- [0..2]]

bingoDiag2 :: BingoSet
bingoDiag2 = [(i, 2 - i) | i <- [0..2]]

bingoAllSet :: [BingoSet]
bingoAllSet =
  [bingoRow i | i <- [0..2]] ++
  [bingoCol j | j <- [0..2]] ++
  [bingoDiag1, bingoDiag2]

type BingoState = [I.IntSet]

createBingoState :: Bingo -> BingoState
createBingoState bingo = map f bingoAllSet
  where
    f bingoSet = I.fromList $ map (bingo M.!) bingoSet

data Answer = No | Yes
  deriving (Show, Eq)

solve :: [Int] -> BingoState -> Answer
solve [] _ = No
solve (x:xs) bingoState =
  let newState = map (I.delete x) bingoState
  in if any I.null newState
     then Yes
     else solve xs newState
