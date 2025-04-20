import Data.List (unfoldr)

type IntX = Int

main :: IO ()
main = do
  n <- readLn

  print $ maximum $ (1:) $ genPowers n

genPowers :: IntX -> [IntX]
genPowers n =
  [ bp
  | b <- [n,(n - 1)..2]
  , bp <- unfoldr (go b) (b * b)
  ]
  where
    go b x
      | n < x = Nothing
      | otherwise = Just (x, b * x)
