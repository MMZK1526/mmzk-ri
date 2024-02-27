import           MMZK.Int.Injection
import           MMZK.Maybe
import           MMZK.RI
import           MMZK.Read
import           MMZK.Read.Internal
import           Test.Hspec
import           Test.QuickCheck
import           Text.Printf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  intInjectionSpec
  maybeSpec
  readSpec
  riSpec

intInjectionSpec :: Spec
intInjectionSpec = describe "MMZK.Int.Injection" do
  describe "Intable for Int types" do
    it "Int" do
      property $ roundtripP @Int
    it "Int8" do
      property $ roundtripP @Int8
    it "Int16" do
      property $ roundtripP @Int16
    it "Int32" do
      property $ roundtripP @Int32
    it "Int64" do
      property $ roundtripP @Int64
  describe "Intable for Word types" do
    it "Word" do
      property $ roundtripP @Word
    it "Word8" do
      property $ roundtripP @Word8
    it "Word16" do
      property $ roundtripP @Word16
    it "Word32" do
      property $ roundtripP @Word32
    it "Word64" do
      property $ roundtripP @Word64
  describe "Intable for other types" do
    it "()" do
      convertToInt () `shouldBe` 0
    it "Bool" do
      convertToInt False `shouldBe` 0
      convertToInt True `shouldBe` 1

roundtripP :: (Eq a, Num a, Intable a) => a -> Bool
roundtripP x = fromIntegral (convertToInt x) == x

maybeSpec :: Spec
maybeSpec = describe "MMZK.Maybe" do
  describe "er2m" do
    it "converts 'Either' to 'Maybe" do
      er2m (Left @String @Bool "foo") `shouldBe` Nothing
      er2m (Right @String @Bool True) `shouldBe` Just True
      el2m (Left @String @Bool "foo") `shouldBe` Just "foo"
      el2m (Right @String @Bool True) `shouldBe` Nothing
    it "convers 'Maybe' to 'Either" do
      m2er @String @Bool "foo" Nothing `shouldBe` Left "foo"
      m2er @String @Bool "foo" (Just True) `shouldBe` Right True
      m2el @String @Bool "foo" Nothing `shouldBe` Right "foo"
      m2el @String @Bool "foo" (Just True) `shouldBe` Left True

readSpec :: Spec
readSpec = describe "MMZK.Read" do
  describe "read Word" do
    it "Word8" do
      map (rdMaybe @Word8) parseWordTests `shouldBe` map (e2m . readWord8Vanilla) parseWordTests
    it "Word16" do
      map (rdMaybe @Word16) parseWordTests `shouldBe` map (e2m . readWord16Vanilla) parseWordTests
    it "Word32" do
      map (rdMaybe @Word32) parseWordTests `shouldBe` map (e2m . readWord32Vanilla) parseWordTests
    it "Word64" do
      map (rdMaybe @Word64) parseWordTests `shouldBe` map (e2m . readWord64Vanilla) parseWordTests
    it "Word" do
      map (rdMaybe @Word) parseWordTests `shouldBe` map (e2m . readWordVanilla) parseWordTests
  describe "read Int" do
    it "Int8" do
      map (rdMaybe @Int8) parseIntTests `shouldBe` map (e2m . readInt8Vanilla) parseIntTests
    it "Int16" do
      map (rdMaybe @Int16) parseIntTests `shouldBe` map (e2m . readInt16Vanilla) parseIntTests
    it "Int32" do
      map (rdMaybe @Int32) parseIntTests `shouldBe` map (e2m . readInt32Vanilla) parseIntTests
    it "Int64" do
      map (rdMaybe @Int64) parseIntTests `shouldBe` map (e2m . readInt64Vanilla) parseIntTests
    it "Int" do
      map (rdMaybe @Int) parseIntTests `shouldBe` map (e2m . readIntVanilla) parseIntTests

parseWordTests :: [String]
parseWordTests = (show @Integer <$> ws)
              ++ (printf "0x%x" <$> ws)
              ++ (printf "0X%X" <$> ws)
  where
    w8   = [0, 2..255]
    w16  = (256 *) <$> w8
    w32  = (256 *) <$> w16
    w64  = (65536 *) <$> w32
    w128 = (4294967296 *) <$> w64
    ws   = [65535, 4294967295, 18446744073709551615] ++ w8 ++ w16 ++ w32 ++ w64 ++ w128

parseIntTests :: [String]
parseIntTests = (show @Integer <$> is)
             ++ (showHexSmall <$> is)
             ++ (showHexBig <$> is)
  where
    showHexSmall x
      | x >= 0    = printf "0x%x" x
      | otherwise = printf "-0x%x" (-x)
    showHexBig x
      | x >= 0    = printf "0X%X" x
      | otherwise = printf "-0X%X" (-x)
    i8   = [-128, -126..127]
    i16  = (256 *) <$> i8
    i32  = (256 *) <$> i16
    i64  = (65536 *) <$> i32
    i128 = (4294967296 *) <$> i64
    is   = [32767, 2147483647, 9223372036854775807] ++ i8 ++ i16 ++ i32 ++ i64 ++ i128

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
