import qualified Data.Sequence as Q
import Data.Sequence (Seq(..))
import qualified Data.IntSet as S
import Data.Foldable (toList)

type IntX = Int

main :: IO ()
main = do
  x <- readLn :: IO IntX

  print $ fromEnum $ solve S.empty x (Q.fromList [0])

items :: [IntX]
items = [100..105]

solve :: S.IntSet -> IntX -> Q.Seq IntX -> Bool
solve _ _ Q.Empty = False
solve memo n (x :<| xs)
  | n == x = True
  | otherwise = solve (memo <> memo') n (xs <> xs')
  where
    xs' = Q.fromList $ filter isOK $ map (+ x) items
    memo' = S.fromList $ toList xs'
    isOK a = (a <= n) && (S.notMember a memo)
