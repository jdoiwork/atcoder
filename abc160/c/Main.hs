import qualified Data.Sequence as S
import Data.Sequence (Seq(..))
import qualified Data.Text.IO as T
import qualified Data.Text as T

main :: IO ()
main = do
  [k, _n] <- readNumbers
  houses <- S.fromList <$> readNumbers
  let housesDis = S.zipWith (-) (rotate k houses) houses
  print (k - maximum housesDis)

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

rotate :: Int -> S.Seq Int -> S.Seq Int
rotate n (x:<|xs) = xs :|> (n + x)
rotate _ _ = undefined
