import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.List (minimumBy)
import Data.Function (on)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  [t, a] <- map (*1000) <$> readNumbers
  hs <- map ((distance a) . (t -) . (*6)) <$> readNumbers
  let answer = fst $ minimumBy (compare `on` snd) $ zip [1..] hs :: Int

  print answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

distance :: IntX -> IntX -> IntX
distance a h = abs (a - h)
