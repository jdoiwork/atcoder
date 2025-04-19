main :: IO ()
main = do
  o <- getLine
  e <- (<> " ") <$> getLine

  putStrLn [c | z <- zip o e, c <- chomp z]

chomp :: (Char, Char) -> String
chomp (a, ' ') = [a]
chomp (a, b) = [a, b]
