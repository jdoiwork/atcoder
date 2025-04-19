import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  bs <- readNumbers
  let as = V.replicate n (10 ^ 5 + 1) :: V.Vector IntX
      query = [(j, b) | (i, b) <- zip [0..] bs, j <- [i, i+1]]
      answer = V.accum min as query

  print $ V.sum answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine
