import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
import Data.List (tails)

type IntX = Int

main :: IO ()
main = do
  [n, d] <- readNumbers
  vs <- replicateM n readNumbers

  let x = [ d
          | (v, ws) <- zip vs $ tail $ tails vs
          , w <- ws
          , let d = distance v w
          , isInt d]
  print $ length x

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

distance2 :: [IntX] -> [IntX] -> IntX
distance2 xs ys = sum $ zipWith (\a b -> (a - b) ^ 2) xs ys

distance :: [IntX] -> [IntX] -> Double
distance xs ys = sqrt $ fromIntegral $ distance2 xs ys

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x)
