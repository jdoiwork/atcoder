import qualified Data.Sequence as Q
import Data.Sequence (Seq(..))
import Data.List (foldl')
import Data.Foldable (toList)

main :: IO ()
main = do
  s <- getLine

  putStrLn $ toList $ foldl' f Q.empty s

f :: Seq Char -> Char -> Seq Char
f Q.Empty   'B' = Q.Empty
f (q :|> _) 'B' = q
f q          c  = q :|> c
