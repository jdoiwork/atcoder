
readNumbersLazy :: T.Text -> [IntX]
readNumbersLazy t
  | T.null t  = []
  | otherwise = case T.decimal t of
    Right (n, rest) -> n : (readNumbersLazy $ T.dropWhile (== ' ') rest)
    Left _ -> []