import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
-- import Control.Monad (replicateM)
-- import System.IO (hPrint, stderr)
import Data.List (foldl')

type IntX = Int

main :: IO ()
main = do
  [_n] <- readNumbers
  monsters <- reverse <$> readNumbers
  heros <- reverse <$> readNumbers

  -- hPrint stderr $ (_n, monsters, heros)
  print $ defeatMonsters monsters heros

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

defeatMonsters :: [IntX] -> [IntX] -> IntX
defeatMonsters (m:monsters) heros = snd $ foldl' go (m, 0) $ zip monsters heros
  where
    go :: (IntX, IntX) -> (IntX, IntX) -> (IntX, IntX)
    go (lastMonsters, kills) (currentMonsters, hero) =
      let
        killedLastMonsters = min lastMonsters hero
        hero' = hero - killedLastMonsters
        killedCurrentMonsters = min currentMonsters hero'
        nextMonsters = currentMonsters - killedCurrentMonsters
      in (nextMonsters, kills + killedLastMonsters + killedCurrentMonsters)
defeatMonsters _ _ = error "Invalid input."
