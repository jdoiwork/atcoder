{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.IntSet as S
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..))
-- import Debug.Trace (traceShow)

type IntX = Int

main :: IO ()
main = do
  [_n, aPos, bPos, aGoal, bGoal] <- readNumbers
  s <- getLine
  let
    maxGoal = max aGoal bGoal
    rocks = S.fromList $ [ i | (i, c) <- zip [1..] s, c == '#', i <= maxGoal ]
    deadRockFree =
      (isDeadRockFree aPos aGoal ((goaled bPos bGoal) <> rocks)) &&
      (isDeadRockFree bPos bGoal ((goaled aPos aGoal) <> rocks))

  T.putStrLn $ case (deadRockFree, moveToSafeZone rocks aPos bPos aGoal bGoal) of
    (False, _)   -> "No"
    (_, Nothing) -> "No"
    (_, _)       -> "Yes"


goaled :: IntX -> IntX -> Rocks
goaled x g = if x == g then [x] else []

isDeadRockFree :: IntX -> IntX -> Rocks -> Bool
isDeadRockFree l r rocks = S.null $ rs `S.intersection` (S.map (+1) rs)
  where
    rs = S.filter (\a -> l <= a && a <= r) rocks

moveToSafeZone :: Rocks -> IntX -> IntX -> IntX -> IntX -> Maybe (IntX, IntX)
moveToSafeZone rocks aPos bPos aGoal bGoal
  | bGoal < aGoal = do
    b' <- swapPosition rocks bGoal aGoal [bPos]
    return (aPos, b')
  | otherwise = return (aPos, bPos)

type Queue1 = Q.Seq IntX
type Rocks = S.IntSet

swapPosition :: Rocks -> IntX -> IntX -> Queue1 -> Maybe IntX
swapPosition _ _ _ Q.Empty = Nothing
swapPosition rocks goal goalA (pos :<| _)
  |    (pos - 1) `S.notMember` rocks
    && (pos + 1) `S.notMember` rocks
              = Just pos
  | otherwise =
    let
      q2 = Q.fromList
        [ p
        | p <- [pos + 1, pos + 2]
        , p <= goal
        , p `S.notMember` rocks
        ]
    in swapPosition rocks goal goalA q2


readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
