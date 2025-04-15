import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- sortBy (compare `on` value) . zip [1..] <$> readNumbers

  putStrLn $ unwords $ map (show . index) xs

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

index :: (Int, Int) -> Int
index = fst

value :: (Int, Int) -> Int
value = snd
