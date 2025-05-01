{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as TB

import Control.Monad (replicateM_)
-- import System.IO (hPrint, stderr)

import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (liftIO)

type IntX = Int

type App = S.StateT (IntX, IntX, IntX, IntX) IO

main :: IO ()
main = do
  [w, h, n] <- readNumbers

  -- hPrint stderr $ (n, w, h)
  y <- S.runStateT (runApp n) (0, 0, w, h)
  -- hPrint stderr $ (y)
  writeNumbers [fst y]


runApp :: IntX -> App IntX
runApp n = do
  replicateM_ n procLine
  calcRect

calcRect :: App IntX
calcRect = do
  (x0, y0, x1, y1) <- S.get
  return $ (max 0 (x1 - x0)) * (max 0 (y1 - y0))

procLine :: App ()
procLine = do
  [x, y, a] <- liftIO readNumbers

  (x0, y0, x1, y1) <- S.get

  S.put $ case a of
    1 -> ((max x0 x), y0, x1, y1)
    2 -> (x0, y0, (min x1 x), y1)
    3 -> (x0, (max y0 y), x1, y1)
    4 -> (x0, y0, x1, (min y1 y))
    _ -> error $ "Invalid Input: a = " ++ show a

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

writeNumbers :: [IntX] -> IO ()
writeNumbers xs = T.putStrLn $ T.unwords $ map (T.toLazyText . TB.decimal) xs
