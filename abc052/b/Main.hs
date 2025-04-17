import Data.List (foldl')

main :: IO ()
main = do
  _n <- readLn :: IO Int
  s <- getLine

  print $ solve $ map convert s

type Func = Int -> Int

inc :: Func
inc n = n + 1

dec :: Func
dec n = n - 1

convert :: Char -> Func
convert 'I' = inc
convert 'D' = dec
convert _ = error "Invalid character"

solve :: [Func] -> Int
solve fs = snd $ foldl' go (0, 0) fs
  where
    go (x, m) f = let y = f x in (y, max m y)
