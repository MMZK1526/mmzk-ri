module Read.Spec where

import           MMZK.Maybe
import           MMZK.Read
import           MMZK.Read.Internal
import           Test.Hspec
import           Text.Printf

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
