
combi :: IntX -> IntX -> IntX
combi a b = x `div` y
  where
    n = min b (a - b)
    x = product $ take n [a, (a-1)..1]
    y = product ([n, (n-1)..1] :: [IntX])
