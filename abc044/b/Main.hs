import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  w <- getLine
  let x = M.fromListWith (+) [(c, 1) | c <- w] :: M.HashMap Char Int

  putStrLn $
    if (M.size $ M.filter even x) == (M.size x)
    then "Yes"
    else "No"
