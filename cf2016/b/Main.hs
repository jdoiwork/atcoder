{-# LANGUAGE CPP #-}

import Data.Foldable (foldlM)

main :: IO ()
main = do
  (n:a:b:_) <- readNumbers
  s <- getLine
  debugPrint (n, a, b, s)
  foldlM run (0, 0, a, b) s
  return ()

readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

type Ctx = (Int, Int, Int, Int)

run :: Ctx -> Char -> IO Ctx
run ctx 'c' = do
  debugPrint ('c', ctx)
  putStrLn "No"
  return ctx
run ctx@(noA_B, noB, a, b) 'a' = do
  debugPrint ('a', ctx)
  if noA_B < a + b
    then do
      putStrLn "Yes"
      return (noA_B + 1, noB, a, b)
    else do
      putStrLn "No"
      return (noA_B, noB, a, b)
run ctx@(noA_B, noB, a, b) 'b' = do
  debugPrint ('b', ctx)
  if noA_B < a + b && noB < b
    then do
      putStrLn "Yes"
      return (noA_B + 1, noB + 1, a, b)
    else do
      putStrLn "No"
      return (noA_B, noB, a, b)

debugPrint :: Show a => a -> IO ()
debugPrint x = do
#ifdef DEBUG
  print x
#endif
  return ()
