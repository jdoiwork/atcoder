import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as VA

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- sortVector . V.fromListN (n * 3) <$> readNumbers

  print $ sum $ take n $ take2s $ V.toList xs

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

sortVector :: V.Vector IntX -> V.Vector IntX
sortVector vec = V.create $ do
  mvec <- V.thaw vec
  VA.sortBy (flip compare) mvec
  return mvec

take2s :: [IntX] -> [IntX]
take2s (_:b:xs) = b : take2s xs
take2s _ = []
