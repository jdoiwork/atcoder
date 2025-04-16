import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [_r, _m, c] <- readNumbers

  refrigerators <- makeTable <$> readNumbers
  microwave <- makeTable <$> readNumbers
  coupons <- replicateM c (makeCoupon <$> readNumbers)

  let simpleCost = V.minimum refrigerators + V.minimum microwave
      couponCosts = map (couponCost refrigerators microwave) coupons
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

type Table = V.Vector Int
makeTable :: [Int] -> Table
makeTable = V.fromList

couponCost :: Table -> Table -> Counpon -> Int
couponCost rTable mTable (Counpon r m d) = rCost + mCost - d
  where
    rCost = rTable V.! (r - 1)
    mCost = mTable V.! (m - 1)
