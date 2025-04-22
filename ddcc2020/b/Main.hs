import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.Sequence as S
import Data.Sequence (Seq(..))

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  q <- S.fromList <$> readNumbers
  let
    (a, b) = split (0, 0) q
    answer = distance a b

  -- hPrint stderr $ (_n, q, split (0, 0) q)
  print answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

split :: (IntX, IntX) -> S.Seq IntX -> (IntX, IntX)
split (a, b) S.Empty = (a, b)
split (a, b) q
  | a <= b = let x :<| q' = q in split (a + x, b) q'
  | a >  b = let q' :|> x = q in split (a, b + x) q'
split _ _ = error "Invalid input"

distance :: IntX -> IntX -> IntX
distance a b = abs $ a - b
