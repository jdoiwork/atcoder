import Control.Monad (replicateM)

main :: IO ()
main = do
  [a, b, c] <- replicateM 3 getLine

  putStrLn [game 'a' a b c]

game :: Char -> String -> String -> String -> Char
game 'a' [] _ _ = 'A'
game 'b' _ [] _ = 'B'
game 'c' _ _ [] = 'C'
game 'a' (a:as) bs cs = game a as bs cs
game 'b' as (b:bs) cs = game b as bs cs
game 'c' as bs (c:cs) = game c as bs cs
game _ _ _ _ = error "Invalid input"
