import qualified Data.Set as S
import Data.List (unfoldr)

main :: IO ()
main = do
  s <- getLine
  print $ maximum $ map length $ splitToChunks s

acgt :: S.Set Char
acgt = S.fromList "ACGT"

isACGT :: Char -> Bool
isACGT c = S.member c acgt

splitToChunks :: String -> [String]
splitToChunks s = unfoldr f s
  where
    f [] = Nothing
    f xs = Just (takeWhile isACGT chunk, tail' chunk)
      where
        chunk = dropWhile (not . isACGT) xs

tail' [] = []
tail' xs = tail xs
