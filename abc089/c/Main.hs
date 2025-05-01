{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.List (sortBy)
import Data.Ord (comparing)

type IntX = Int

main :: IO ()
main = do
  [n] <- readNumbers
  ss <- onlyMarch <$> replicateM n T.getLine
  let
    table = M.fromListWith (+) [(T.head s, 1) | s <- ss]

  -- hPrint stderr $ (n, ss, table)

  writeNumbers [select table]

type Table = M.HashMap Char IntX

select :: Table -> IntX
select table = sum ns
  where
    xs = sortBy (comparing fst) $ M.toList table
    ns =
      [ ax * bx * cx
      | (ai, ax) <- xs
      , (bi, bx) <- xs
      , ai < bi
      , (ci, cx) <- xs
      , bi < ci
      ]

initials :: S.HashSet Char
initials = ['M', 'A', 'R', 'C', 'H']

onlyMarch :: [T.Text] -> [T.Text]
onlyMarch = filter ((`S.member` initials) . T.head)

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
