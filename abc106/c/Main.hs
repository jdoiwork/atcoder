main :: IO ()
main = do
  s <- getLine
  k <- readLn

  let
    n1s = length $ takeWhile (== '1') s
    s' = take 1 $ drop n1s s

  if k <= n1s
    then print 1
    else putStrLn s'
