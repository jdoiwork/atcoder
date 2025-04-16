import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (sort)

main :: IO ()
main = do
  [_n, x] <- readNumbers
  children <- sort <$> readNumbers
  let state = foldl distribute (x, []) children
      answer = length $ filter isSatisfied $ finish state

  print answer

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

type Want = Int
type Have = Int
type Child = (Want, Have)
type Count = Int
type DistState = (Count, [Child])

distribute :: DistState -> Want -> DistState
distribute (count, cs) want = (count - have, (want, have):cs)
  where
    have = min count want

finish :: DistState -> [Child]
finish (count, (want, have):cs) = (want, have + count):cs
finish _ = []

isSatisfied :: Child -> Bool
isSatisfied (want, have) = want == have
