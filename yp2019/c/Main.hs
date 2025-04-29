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
  [count, bis2money, money2bis] <- readNumbers

  -- hPrint stderr $ (count, bis2money, money2bis)

  let
    bis2bis = max 2 $ money2bis - bis2money
    countToA = bis2money - 1

  -- hPrint stderr $ ("bis2bis, countToA", bis2bis, countToA)

  if count < countToA
    then writeNumbers [count]
    else writeNumbers [solve (count - countToA) bis2bis bis2money]


type Count = IntX
type Step2 = IntX

solve :: Count -> Step2 -> IntX -> IntX
solve count s2 bis = bis + (s2 * (count `div` 2)) + (count `mod` 2)

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
