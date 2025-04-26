{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

type IntX = Int

main :: IO ()
main = do
  s <- T.dropWhile isW <$> T.getLine

  print $ solve 0 0 s
  -- print $ solve 0 0 $ T.replicate 10000 "BW"

solve :: IntX -> IntX -> T.Text -> IntX
solve !bs !n t =
  case T.breakOn "W" t of
    (_, "") -> n
    (bs', wbs) ->
      let lbs = len bs'
      in solve (bs + lbs) (n + bs + lbs) $ T.tail wbs

isW :: Char -> Bool
isW c = c == 'W'

len :: T.Text -> IntX
len = fromIntegral . T.length
