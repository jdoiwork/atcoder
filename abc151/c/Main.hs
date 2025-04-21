{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import Data.List (foldl')
import Text.Printf (printf)

type IntX = Int

main :: IO ()
main = do
  [_n, m] <- readNumbers
  posts <- replicateM m readPost

  let accum = foldl' update (S.empty, M.empty) posts
      answer = createAnswer accum

  -- hPrint stderr $ (n, posts, accum, answer)
  putStrLn $ (uncurry printAnswer) answer

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

data Result = AC | WA deriving (Show, Eq)

data Post = Post
  { postId :: !IntX
  , postResult :: !Result
  }
  deriving (Show, Eq)

toResult :: T.Text -> Result
toResult "AC" = AC
toResult "WA" = WA
toResult _    = error "Invalid result"

readPost :: IO Post
readPost = do
  [index, result] <- T.words <$> T.getLine
  return $ Post (read (T.unpack index)) (toResult result)

type Accum = (S.IntSet, M.IntMap IntX)

update :: Accum -> Post -> Accum
update (acs, waCounts) (Post index AC) = (S.insert index acs, waCounts)
update (acs, waCounts) (Post index WA)
  | S.member index acs = (acs, waCounts)
  | otherwise          = (acs, M.insertWith (+) index 1 waCounts)

createAnswer :: Accum -> (IntX, IntX)
createAnswer (acs, waCounts) = (acCount, allWaCount)
  where
    acCount = S.size acs
    allWaCount = M.foldrWithKey go 0 waCounts
    go index waCount acc
      | S.member index acs = acc + waCount
      | otherwise          = acc

printAnswer :: IntX -> IntX -> String
printAnswer = printf "%d %d"
