{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

-- import System.IO (hPrint, stderr)

import qualified Data.Vector.Unboxed as V

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  xs <- V.fromListN n <$> readNumbers
  let
    (a, b) = V.splitAt 1 xs
    seed = (V.sum a, V.sum b)
    ys = V.scanl' exchange seed $ V.init b

  -- hPrint stderr $ (n, xs, a, b, seed, ys)
  writeNumbers [V.minimum $ V.map distance ys]

exchange :: (IntX, IntX) -> IntX -> (IntX, IntX)
exchange (a, b) x = (a + x, b - x)

distance :: (IntX, IntX) -> IntX
distance (x, y) = abs $ x - y

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
