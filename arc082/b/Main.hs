{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  xs <- readNumbers

  -- hPrint stderr $ (n, xs)
  writeNumbers [solve (0, 0) 1 xs]

solve :: (IntX, IntX) -> IntX -> [IntX] -> IntX
solve !(n2, n1) _ [] = n2 + n1

solve !(n2, n1) !i (x1:[])
  | x1 == i = solve (n2, n1 + 1) (i + 1) []
  | x1 /= i = solve (n2, n1) (i + 1) []

solve !(n2, n1) !i (x1:x2:xs)
  | x1 == i && x2 == (i + 1) = solve (n2 + 1, n1) (i + 2) xs
  | x1 == i && x2 /= (i + 1) = solve (n2, n1 + 1) (i + 2) xs
  | x1 /= i && x2 == (i + 1) = solve (n2, n1) (i + 1) (x2:xs)
  | x1 /= i && x2 /= (i + 1) = solve (n2, n1) (i + 2) xs

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
