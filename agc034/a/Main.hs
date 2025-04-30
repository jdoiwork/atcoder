{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)

import qualified Data.IntSet as S
import qualified Data.HashSet as HS
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
      (isDeadRockFree aPos aGoal ((goaled bPos bGoal) <> rocks))
      && (isDeadRockFree bPos bGoal ((goaled aPos aGoal) <> rocks))

  -- hPrint stderr $ (n, aPos, bPos, aGoal, bGoal, s, rocks)
  -- T.putStrLn $ if solve [] rocks aGoal bGoal [(aPos, bPos)]

  T.putStrLn $ case (deadRockFree, moveToSafeZone rocks aPos bPos aGoal bGoal) of
    (False, _) -> "No"
    (_, Nothing) -> "No"
    (_, _) -> "Yes"

  -- T.putStrLn $ if solve [] rocks aGoal bGoal [(aPos, bPos)]
  --   then "Yes"
  --   else "No"

goaled :: IntX -> IntX -> Rocks
goaled x g = if x == g then [x] else []

isDeadRockFree :: IntX -> IntX -> Rocks -> Bool
isDeadRockFree l r rocks = S.null $ rs `S.intersection` (S.map (+1) rs)
  where
    rs = S.filter (\a -> l <= a && a <= r) rocks
moveToSafeZone :: Rocks -> IntX -> IntX -> IntX -> IntX -> Maybe (IntX, IntX)
moveToSafeZone rocks aPos bPos aGoal bGoal
  | bGoal < aGoal = do
    b' <- swapPosition [] rocks bGoal aGoal [bPos]
    return (aPos, b')
  | otherwise = return (aPos, bPos)

type Pos = (IntX, IntX)
type Queue = Q.Seq Pos
type Queue1 = Q.Seq IntX
type Rocks = S.IntSet
type History = HS.HashSet Pos
type History1 = S.IntSet

swapPosition :: History1 -> Rocks -> IntX -> IntX -> Queue1 -> Maybe IntX
swapPosition _ _ _ _ Q.Empty = Nothing
swapPosition hist rocks goal goalA (pos :<| q)
  | goal == pos = Just pos
  |    (pos - 1) `S.notMember` rocks
    && (pos + 1) `S.notMember` rocks
              = Just pos
  | otherwise =
    let
      newHist = S.insert pos hist
      q2 = Q.fromList
        [ p
        | p <- [pos + 1, pos + 2]
        , p < goal
        , p `S.notMember` rocks
        , p `S.notMember` hist
        ]
    in swapPosition newHist rocks goal goalA (q <> q2)

solve :: History -> Rocks -> IntX -> IntX -> Queue -> Bool
solve _ _ _ _ Q.Empty = False
solve _ _ aGoal bGoal ((aPos, bPos) :<| _)
  | aGoal == aPos && bGoal == bPos = True
solve history rocks aGoal bGoal (pos :<| q) =
  let
    validPos (a, b) =
      (a /= b)
      && (a <= aGoal)
      && (b <= bGoal)
      && (a `S.notMember` rocks)
      && (b `S.notMember` rocks)
      && (not $ (a, b) `HS.member` history)
    history' = HS.insert pos history
    q2 = Q.filter validPos $ nexts pos
  in
    -- traceShow ("q2", q2) $
    solve history' rocks aGoal bGoal $ q2 <> q

nexts :: Pos -> Queue
nexts (a, b) =
  [ (a + 2, b)
  , (a + 1, b)
  , (a, b + 2)
  , (a, b + 1)
  ]

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e
