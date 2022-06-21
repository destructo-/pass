module Domain.Utils.ListUtilsSpec (spec) where

import Test.Hspec (
    context
  , describe
  , it
  , parallel
  , shouldBe
  , Spec )

import qualified Domain.Utils.ListUtils as ListUtils


spec :: Spec
spec = parallel $ do
    describe "ListUtils" $ do
        context "maybeHead function" $ do
            it "can take head from non empty list and return Just a" $ do
                ListUtils.maybeHead [23..] `shouldBe` Just 23

            it "can take head from empty list and return Nothing" $ do
                (ListUtils.maybeHead [] :: Maybe Int) `shouldBe` Nothing

        context "safeTail function" $ do
            it "can take tail from non empty list as usual tail" $ do
                ListUtils.safeTail [23, 24, 25] `shouldBe` [24, 25]

            it "can take tail from empty list and return []" $ do
                (ListUtils.safeTail [] :: [Int]) `shouldBe` ([] :: [Int])