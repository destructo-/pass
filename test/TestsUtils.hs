module TestsUtils (checkRecord) where

import Test.Hspec ( shouldBe, Expectation )

import Domain.Record (Record(..))


checkRecord :: Maybe Record -> Record -> Expectation
checkRecord (Just record) expectedRecord = do
    name record `shouldBe` name expectedRecord
    password record `shouldBe` password expectedRecord
    mark record `shouldBe` mark expectedRecord
checkRecord Nothing expectedRecord =
    Nothing `shouldBe` Just expectedRecord
