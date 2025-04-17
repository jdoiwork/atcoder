import qualified Data.Set as S

main :: IO ()
main = do
  s <- getLine

  putStrLn $ if (length $ S.fromList s) == length s
    then "yes"
    else "no"
