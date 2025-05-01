{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T

import Data.Attoparsec.Text.Lazy

import Control.Monad (void)
import Control.Applicative ((<|>))
-- import System.IO (hPrint, stderr)
import Control.Monad.State.Strict as S
import Control.Lens (makeLenses, use, (%=))

type IntX = Int

data AppContext = AppContext
  { _lBuffer :: String
  , _rBuffer :: String
  , _center :: T.Text
  , _reversed :: !Bool
  } deriving (Show)

makeLenses ''AppContext

main :: IO ()
main = do
  s <- T.getLine
  [n] <- readNumbers

  y <- S.evalStateT (runApp n) $ AppContext [] [] s False
  T.putStrLn y


type App = S.StateT AppContext IO

runApp :: Int -> App T.Text
runApp 0 = closeContext
runApp !n = do
  t <- liftIO T.getLine
  case parseOnly parser t of
    Left _ -> return "ERROR"
    Right q -> do
      updateContext q

      runApp (n - 1)

closeContext :: App T.Text
closeContext = do
  l <- T.pack <$> use lBuffer
  r <- T.pack . reverse <$> use rBuffer
  isFlipped <- use reversed
  t <- use center

  let t' = (if isFlipped then T.reverse else id) t
  return $ l <> t' <> r

updateContext :: Query -> App ()
updateContext Reverse = do
  S.modify $ \(ctx@AppContext{..}) -> ctx
    { _lBuffer = _rBuffer
    , _rBuffer = _lBuffer
    , _reversed = not _reversed
    }
updateContext (Cons c) = lBuffer %= (c:)
updateContext (Snoc c) = rBuffer %= (c:)


data Query
  = Reverse
  | Cons Char
  | Snoc Char
  deriving (Show)

parser = do
  -- query `sepBy1` endOfLine
  -- return [query]
  query

query = queryReverse <|> queryCons <|> querySnoc

queryReverse = do
  void $ char '1'
  return Reverse

queryCons = do
  void $ char '2'
  void space
  void $ char '1'
  void space
  c <- anyChar
  return $ Cons c

querySnoc = do
  void $ char '2'
  void space
  void $ char '2'
  void space
  c <- anyChar
  return $ Snoc c

readNumbers :: IO [IntX]
readNumbers = map readDecimal . T.words <$> T.getLine
  where
    readDecimal t = case T.signed T.decimal t of
      Right (!n, _) -> n
      Left e -> error $ "Invalid Input: " ++ show e

