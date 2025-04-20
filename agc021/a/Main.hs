import Data.List (unfoldr)

type IntX = Int

main :: IO ()
main = do
  s <- getLine
  let l = length s
      n = read s :: IntX
      nHead = read $ take 1 s :: IntX
      answer = maximum $
        [ sumDigits n
        , (nHead - 1) + 9 * (l - 1)
        ]

  print answer

sumDigits :: IntX -> IntX
sumDigits n = sum $ unfoldr go n
  where
    go 0 = Nothing
    go x = Just (x `mod` 10, x `div` 10)
