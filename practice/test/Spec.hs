import Test.Hspec
import Main (ioSort)

main :: IO ()
main = hspec $ do
  describe "ioSort" $ do
    it "sorts a simple list of characters" $ do
      result <- ioSort "CBA"
      result `shouldBe` "ABC"

    it "handles an empty list" $ do
      result <- ioSort ""
      result `shouldBe` ""

    it "sorts a list with duplicate characters" $ do
      result <- ioSort "AABBC"
      result `shouldBe` "AABBC"