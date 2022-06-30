module Domain.RecordsSeqSpec (spec) where

import Test.Hspec (
    shouldBe
  , parallel
  , it
  , describe
  , Spec)

import qualified TestsUtils
import qualified Config
import           Domain.Record (Record(..), createRecord)
import Domain.RecordsSeq (
    RecordsSeq
  , writeRecordsSeq
  , readRecordsSeq
  , addIfNotExist
  , removeIfExist
  , findRecord
  , getNamesList )


spec :: Spec
spec = parallel $ do
    describe "read and write operations" $ do
        it "must correctly read the written data" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let recordsSeq = [record0, record1, record2]
            writtenRecords <- writeRecordsSeq recordsSeq Config.lineDevider Config.recordDevider
            readedRecords <- readRecordsSeq writtenRecords Config.lineDevider Config.recordDevider
            length readedRecords `shouldBe` 3
            Just (readedRecords !! 0) `TestsUtils.checkRecord` record0
            Just (readedRecords !! 1) `TestsUtils.checkRecord` record1
            Just (readedRecords !! 2) `TestsUtils.checkRecord` record2

    describe "adding elements" $ do
        it "must be idempotent for record name" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let newRecordWithExistedName = createRecord "first" "some password" (Just "mark")
            let recordsSeq = [record0, record1, record2]
            let updatedRecordsSeq = newRecordWithExistedName `addIfNotExist` recordsSeq
            length updatedRecordsSeq `shouldBe` 3

        it "must add new element to head of a RecordsSeq" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let recordsSeq = [record1, record2]
            let updatedRecordsSeq = record0 `addIfNotExist` recordsSeq
            length updatedRecordsSeq `shouldBe` 3
            Just (head updatedRecordsSeq) `TestsUtils.checkRecord` record0

    describe "deleting elements" $ do
        it "must delete coresponded element" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let recordsSeq = [record0, record1, record2]
            let updatedRecordsSeq = (name record2) `removeIfExist` recordsSeq
            length updatedRecordsSeq `shouldBe` 2
            Just (updatedRecordsSeq !! 0) `TestsUtils.checkRecord` record0
            Just (updatedRecordsSeq !! 1) `TestsUtils.checkRecord` record1

        it "do nothing if element is not in RecordsSeq" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let recordsSeq = [record0, record1]
            let updatedRecordsSeq = (name record2) `removeIfExist` recordsSeq
            length updatedRecordsSeq `shouldBe` 2
            Just (updatedRecordsSeq !! 0) `TestsUtils.checkRecord` record0
            Just (updatedRecordsSeq !! 1) `TestsUtils.checkRecord` record1

    describe "search record" $ do
        it "must return Just Record if exist" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let recordsSeq = [record0, record1, record2]
            let findedRecord = "first" `findRecord` recordsSeq
            findedRecord `TestsUtils.checkRecord` record0

        it "must return Nothing if doesn't exist" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let recordsSeq = [record0, record1, record2]
            let findedRecord = "not_exist" `findRecord` recordsSeq
            findedRecord `shouldBe` Nothing

    describe "when build names list from RecordsSeq" $ do
        it "must create full names list with indexes starts from 1" $ do
            let record0 = createRecord "first" "password 1" Nothing
            let record1 = createRecord "second" "password" (Just "mark")
            let record2 = createRecord "third" "password" Nothing
            let recordsSeq = [record0, record1, record2]
            let namesList = getNamesList recordsSeq
            length namesList `shouldBe` 3
            namesList !! 0 `shouldBe` (1, "first")
            namesList !! 1 `shouldBe` (2, "second")
            namesList !! 2 `shouldBe` (3, "third")

