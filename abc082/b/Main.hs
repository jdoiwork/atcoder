import Data.List (sortBy)

main :: IO ()
main = do
  s <- getLine
  t <- getLine

  putStrLn $ if (sortBy compare s) < (sortBy (flip compare) t)
    then "Yes"
    else "No"
