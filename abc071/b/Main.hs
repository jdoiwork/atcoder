import qualified Data.Set as S

main :: IO ()
main = do
  s <- S.fromList <$> getLine
  let az = S.fromList ['a'..'z']
      x = S.difference az s

  case S.null x of
    True  -> putStr "None"
    False -> putChar $ minimum x

  putStrLn ""
