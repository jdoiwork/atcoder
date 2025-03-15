import Control.Monad.Extra (partitionM)
import System.IO (hFlush, stdout)
import Data.Foldable (foldrM, foldlM)

main :: IO ()
main = do
  (n:q:_) <- readNumbers
  let cs = take n ['A'..'Z']

  -- sortedCs <- ioSort cs
  res <- foldlM ioInsert [] cs

  putStr "! "
  -- putStrLn sortedCs
  putStrLn res

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

ioSort :: [Char] -> IO [Char]
ioSort [] = return []
ioSort (c:cs) = do
  (small, large) <- partitionM (ioCompare c) cs
  sortedSmall <- ioSort small
  sortedLarge <- ioSort large
  return $ concat [sortedSmall, [c], sortedLarge]

ioCompare :: Char -> Char -> IO Bool
ioCompare a b = do
  putStrLn $ ['?', ' ', a, ' ', b]
  ioFlush
  c <- getLine
  return (c /= "<")

ioFlush :: IO ()
ioFlush = hFlush stdout

ioInsert :: [Char] -> Char -> IO [Char]
ioInsert [] c = return [c]
ioInsert (c:cs) x = do
  b <- ioCompare c x
  case b of
    True -> return (x:c:cs)
    False -> (c:) <$> ioInsert cs x
