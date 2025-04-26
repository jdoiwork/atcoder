import qualified Data.Text.IO as T
import qualified Data.Text as T

type IntX = Int

main :: IO ()
main = do
  s <- T.getLine
  let t = T.dropEnd (if even (T.length s) then 2 else 1) s

  print $ solve t

solve :: T.Text -> IntX
solve t =
  let
    n = T.length t `div` 2
    (a, b) = T.splitAt n t
  in if a == b
    then fromIntegral $ T.length t
    else solve $ T.dropEnd 2 t
