import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Search as V
import qualified Data.Vector.Algorithms.Intro as V

search :: V.Vector IntX -> IntX -> Int
search xs x = runST $ do
  mvec <- V.unsafeThaw xs
  V.binarySearch mvec x

sortBy :: (IntX -> IntX -> Ordering) -> V.Vector IntX -> V.Vector IntX
sortBy cmp xs = V.create $ do
  mvec <- V.thaw xs
  V.sortBy cmp mvec
  return mvec
