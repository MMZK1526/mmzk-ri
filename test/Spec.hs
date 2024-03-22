import           Int.Injection.Spec
import           Maybe.Spec
import           Read.Spec
import           MMZK.List
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  intInjectionSpec
  maybeSpec
  readSpec
  riSpec

riSpec :: Spec
riSpec = describe "MMZK.RI" do
  describe "lenCompare" do
    it "compares lists by their length" do
      lenCompare @Int @Integer [1, 2, 3] [1, 2, 3] `shouldBe` EQ
      lenCompare @Int @Integer [1, 2, 3] [1, 2] `shouldBe` GT
      lenCompare @Int @Integer [1, 2] [1, 2, 3] `shouldBe` LT
      lenCompare @Int @Integer [] [1, 2, 3] `shouldBe` LT
      lenCompare @Int @Integer [1, 2, 3] [] `shouldBe` GT
      lenCompare @Int @Integer [] [] `shouldBe` EQ
  describe "lenCompareNum" do
    it "compares lists by their length" do
      lenCompareNum @Int @Int [1, 2, 3] 3 `shouldBe` EQ
      lenCompareNum @Int @Int [1, 2, 3] 2 `shouldBe` GT
      lenCompareNum @Int @Int [1, 2] 3 `shouldBe` LT
      lenCompareNum @Int @Int [] 3 `shouldBe` LT
      lenCompareNum @Int @Int [1, 2, 3] 0 `shouldBe` GT
      lenCompareNum @Int @Int [] 0 `shouldBe` EQ
      lenCompareNum @Int @Int [] -1 `shouldBe` GT
  describe "notNull" do
    it "checks empty lists" do
      notNull [] `shouldBe` False
    it "checks non-empty lists" do
      notNull [True] `shouldBe` True
      notNull [False, False] `shouldBe` True
