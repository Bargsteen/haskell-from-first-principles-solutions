module RepeatedAddition where

import           Test.Hspec

testRepAdd :: IO()
testRepAdd = hspec $ do
  describe "Mult via repeated addition" $ do
    it "10 times 4 is 40" $ do
      repAddMultiply 10 4 `shouldBe` 40
    it "2 times -2 is -4" $ do
      repAddMultiply 2 (-2) `shouldBe` (-4)
    it "-3 times -3 is 9" $ do
      repAddMultiply (-3) (-3) `shouldBe` 9
    it "-4 times -3 is equal\
       \ to -3 times -4" $ do
      repAddMultiply (-4) (-3) `shouldBe` repAddMultiply (-3) (-4)
    it "10 times 0 is 0" $ do
      repAddMultiply 10 0 `shouldBe` 0


repAddMultiply :: Integral a => a -> a -> a
repAddMultiply x y = go 0 x y
  where go sum z n
         | n < 0 = go sum (-z) (-n)
         | n > 0 = go (sum + z) z (n-1)
         | n == 0 = sum
