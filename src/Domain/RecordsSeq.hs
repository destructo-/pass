module Domain.RecordsSeq (
    LineDevider
  , RecordsSeq
  , addIfNotExist
  , removeIfExist
  , findRecord
  , readRecordsSeq
  , writeRecordsSeq
) where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

import qualified Domain.Utils.ListUtils as ListUtils
import Domain.Record (Record(..), Name, RecordDevider, readRecord, writeRecord)
import Data.List (intercalate)


type LineDevider = String

type RecordsSeq = [Record]


addIfNotExist :: Record -> RecordsSeq -> RecordsSeq
addIfNotExist record recordsSeq =
    if record `elem` recordsSeq
        then recordsSeq
        else record : recordsSeq


removeIfExist :: Name -> RecordsSeq -> RecordsSeq
removeIfExist recordName = filter (\r -> name r /= recordName)


findRecord :: Name -> RecordsSeq -> Maybe Record
findRecord recordName records =
    ListUtils.maybeHead $ filter (\r -> name r == recordName) records


readRecordsSeq :: String -> LineDevider -> RecordDevider -> RecordsSeq
readRecordsSeq str lineDevider recordDevider =
    mapMaybe (`readRecord` recordDevider) (splitOn lineDevider str)


writeRecordsSeq :: RecordsSeq -> LineDevider -> RecordDevider -> String
writeRecordsSeq recordsSeq lineDevider recordDevider =
    lineDevider `intercalate` map (`writeRecord` recordDevider) recordsSeq