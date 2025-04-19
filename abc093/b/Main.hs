import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  [a, b, k] <- readNumbers
  let smalls = take k [a..b]
      bigs   = take k [b, b-1..(a+k)]

  mapM_ print $ smalls
  mapM_ print $ reverse bigs

readNumbers :: IO [IntX]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

