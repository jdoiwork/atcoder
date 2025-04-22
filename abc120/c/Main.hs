import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  s <- T.getLine
  let answer = T.foldl' merge (0, []) s

  print $ fst answer

merge :: (IntX, String) -> Char -> (IntX, String)
merge (n, []) c = (n, [c])
merge (n, ('0':cs)) '1' = (n + 2, cs)
merge (n, ('1':cs)) '0' = (n + 2, cs)
merge (n, cs) c = (n, c:cs)
