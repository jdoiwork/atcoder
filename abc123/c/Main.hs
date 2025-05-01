import qualified Data.Vector.Unboxed as V

-- import System.IO (hPrint, stderr)

type IntX = Int

numberOfIslands :: IntX
numberOfIslands = 5

main :: IO ()
main = do
  n <- readLn :: IO IntX
  xs <- V.replicateM numberOfIslands readLn

  let
    minV = V.minimum xs
    packets = calcPackets n minV

  -- hPrint stderr $ (n, xs :: V.Vector IntX, minV, packets)
  print $ numberOfIslands + packets - 1

calcPackets :: IntX -> IntX -> IntX
calcPackets n size = ceiling $ (fromIntegral n / fromIntegral size :: Double)
