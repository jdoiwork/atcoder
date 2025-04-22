import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Text.Printf (printf)


type IntX = Int

main :: IO ()
main = do
  [a, b, c, d] <- readNumbers
  let (x1, x2, x3) = solve (a, b, c, d)

  -- hPrint stderr $ (a, b, c, d)
  -- hPrint stderr $ (x1, x2, x3)
  putStrLn $ formatAnswer (a, b, c, d) (x1, x2, x3)

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.chunksOf 1 <$> T.getLine

solve :: (IntX, IntX, IntX, IntX) -> (Char, Char, Char)
solve (a, b, c, d) = head
  [ (x1, x2, x3)
  | (x1, op1) <- ops
  , (x2, op2) <- ops
  , (x3, op3) <- ops
  , a `op1` b `op2` c `op3` d == 7
  ]
  where
    ops = [('+', (+)), ('-', (-))]

formatAnswer :: (IntX, IntX, IntX, IntX) -> (Char, Char, Char) -> String
formatAnswer (a, b, c, d) (x1, x2, x3) = printf "%d%c%d%c%d%c%d=7" a x1 b x2 c x3 d
