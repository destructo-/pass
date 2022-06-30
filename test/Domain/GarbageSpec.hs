module Domain.GarbageSpec (spec) where

import Test.Hspec (
    context
  , describe
  , it
  , parallel
  , shouldBe
  , Spec, shouldNotBe )

import Domain.Garbage (generateGarbageStrings)


spec :: Spec
spec = parallel $ do
    describe "One time generated strings" $ do
        it "should non be equal" $ do
            strs <- generateGarbageStrings 2 (5, 10)
            length strs `shouldBe` 2
            (strs !! 0) `shouldNotBe` (strs !! 1)

        it "length of strings shold be between range min and range max" $ do
            strs <- generateGarbageStrings 30 (5, 10)
            length strs `shouldBe` 30
            minimum (map length strs) >= 5 `shouldBe` True
            maximum (map length strs) <= 10 `shouldBe` True

