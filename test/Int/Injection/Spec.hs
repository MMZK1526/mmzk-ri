module Int.Injection.Spec where

import           MMZK.Int.Injection
import           Test.Hspec
import           Test.QuickCheck

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
