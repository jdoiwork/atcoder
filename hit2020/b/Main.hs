import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad (replicateM)
import qualified Data.Array as A

main :: IO ()
main = do
  [r, m, c] <- readNumbers

  refrigerators <- readNumbers
  microwave <- readNumbers
  coupons <- replicateM c (makeCoupon <$> readNumbers)

  let rTable = makeTable r refrigerators
      mTable = makeTable m microwave
      simpleCost = minimum refrigerators + minimum microwave
      couponCosts = map (couponCost rTable mTable) coupons
      minCost = minimum $ simpleCost : couponCosts

  print minCost

readNumbers :: IO [Int]
readNumbers = map (read . T.unpack) . T.words <$> T.getLine

data Counpon = Counpon
  { indexR :: Int
  , indexM :: Int
  , discount :: Int
  } deriving (Show)

makeCoupon :: [Int] -> Counpon
makeCoupon [r, m, d] = Counpon r m d
makeCoupon _ = error "Invalid coupon format"

type Table = A.Array Int Int
makeTable :: Int -> [Int] -> Table
makeTable n xs = A.listArray (1, n) xs

couponCost :: Table -> Table -> Counpon -> Int
couponCost rTable mTable (Counpon r m d) = rCost + mCost - d
  where
    rCost = rTable A.! r
    mCost = mTable A.! m
