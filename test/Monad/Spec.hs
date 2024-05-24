module Monad.Spec (monadSpec) where

import           MMZK.Maybe
import           Test.Hspec

monadSpec :: Spec
monadSpec = describe "MMZK.Maybe" do
  describe "er2m" do
    it "converts 'Either' to 'Maybe" do
      er2m (Left @String @Bool "foo") `shouldBe` Nothing
      er2m (Right @String @Bool True) `shouldBe` Just True
      el2m (Left @String @Bool "foo") `shouldBe` Just "foo"
      el2m (Right @String @Bool True) `shouldBe` Nothing
    it "converts 'Maybe' to 'Either" do
      m2er @String @Bool "foo" Nothing `shouldBe` Left "foo"
      m2er @String @Bool "foo" (Just True) `shouldBe` Right True
      m2el @String @Bool "foo" Nothing `shouldBe` Right "foo"
      m2el @String @Bool "foo" (Just True) `shouldBe` Left True
