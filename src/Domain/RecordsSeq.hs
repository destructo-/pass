module Domain.RecordsSeq (
    LineDevider
  , RecordsSeq
  , addIfNotExist
  , removeIfExist
  , findRecord
  , readRecordsSeq
  , writeRecordsSeq
  , getNamesList
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


getNamesList :: RecordsSeq -> [(Int, Name)]
getNamesList recsSeq = zip [1..] $ map name recsSeq


findRecord :: Name -> RecordsSeq -> Maybe Record
findRecord recordName records =
    ListUtils.maybeHead $ filter (\r -> name r == recordName) records


readRecordsSeq :: String -> LineDevider -> RecordDevider -> IO RecordsSeq
readRecordsSeq str lineDevider recordDevider =
    pure $ mapMaybe (`readRecord` recordDevider) (splitOn lineDevider str)


writeRecordsSeq :: RecordsSeq -> LineDevider -> RecordDevider -> IO String
writeRecordsSeq recordsSeq lineDevider recordDevider =
    pure $ lineDevider `intercalate` map (`writeRecord` recordDevider) recordsSeq