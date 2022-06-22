module Domain.RecordSpec (spec) where

import Test.Hspec (
    context
  , describe
  , it
  , parallel
  , shouldBe
  , Spec
  , Expectation )

import qualified TestsUtils
import           Domain.Record (Record(..), readRecord, createRecord, writeRecord)
import qualified Config


spec :: Spec
spec = parallel $ do
    describe "Records equality" $ do
        it "two records are considered equal if their names are equal" $ do
            let name = "Name"
            let firstRecord = createRecord name "first_password" (Just "mark")
            let secondRecord = createRecord name "second_password" Nothing
            (firstRecord == secondRecord) `shouldBe` True

        it "two records are considered not equal if their names are not equal" $ do
            let firstRecord = createRecord "first_name" testPass (Just testMark)
            let secondRecord = createRecord "second_name" testPass (Just testMark)
            (firstRecord == secondRecord) `shouldBe` False


    describe "when read record from correct string" $ do
        it "must create corresponding Record for key-pass string" $ do
            let keyPassStr = createRecordString testName testPass Nothing
            let expectedRecord = createRecord testName testPass Nothing
            let record = readRecord keyPassStr Config.recordDevider
            record `TestsUtils.checkRecord` expectedRecord

        it "must create corresponding Record for key-pass-mark string" $ do
            let keyPassStr = createRecordString testName testPass (Just testMark)
            let expectedRecord = createRecord testName testPass (Just testMark)
            let record = readRecord keyPassStr Config.recordDevider
            record `TestsUtils.checkRecord` expectedRecord

        it "must skip redundant data when create Record" $ do
            let redudant = Config.recordDevider ++ "redudant_data"
            let keyPassStr = createRecordString testName testPass (Just testMark)
            let expectedRecord = createRecord testName testPass (Just testMark)
            let record = readRecord (keyPassStr ++ redudant) Config.recordDevider
            record `TestsUtils.checkRecord` expectedRecord

    describe "when read record from incorrect string" $ do
        it "must return Nothing" $ do
            let incorrectString = "incorrect_string"
            let record = readRecord incorrectString Config.recordDevider
            record `shouldBe` Nothing

    describe "when write record" $ do
        it "must create correct string for Record with name and pass" $ do
            let record = createRecord testName testPass Nothing
            let expectedString = createRecordString testName testPass Nothing
            let string = writeRecord record Config.recordDevider
            string `shouldBe` expectedString

        it "must create correct string fo Record with name pass and mark" $ do
            let record = createRecord testName testPass (Just testMark)
            let expectedString = createRecordString testName testPass (Just testMark)
            let string = writeRecord record Config.recordDevider
            string `shouldBe` expectedString


testName :: String
testName = "test_name"


testPass :: String
testPass = "test_pass"


testMark :: String
testMark = "test_mark"


createRecordString :: String -> String -> Maybe String -> String
createRecordString fst scd (Just srd) =
    fst ++ Config.recordDevider ++ scd ++ Config.recordDevider ++ srd
createRecordString fst scd Nothing =
    fst ++ Config.recordDevider ++ scd