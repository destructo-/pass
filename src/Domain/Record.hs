module Domain.Record (
    Record(..)
  , Name
  , Mark
  , Password
  , RecordDevider
  , readRecord
  , writeRecord
  , createRecord
) where

import Data.List.Split (splitOn)
import Control.Concurrent (yield)


type RecordDevider = String

type Name = String
type Password = String
type Mark = String

data Record = Record {
    name :: Name
  , password :: Password
  , mark :: Maybe Mark }
  deriving (Show)


instance Eq Record where
  (==) x y = name x == name y
  (/=) x y = not $ x == y


createRecord :: Name -> Password -> Maybe Mark -> Record
createRecord name pass mark =
    Record {
        name = name
      , password = pass
      , mark = mark }


readRecord :: String -> RecordDevider -> Maybe Record
readRecord str devider = _createRecordFrom $ splitOn devider str


writeRecord :: Record -> RecordDevider -> String
writeRecord (Record name password (Just login)) devider =
    name ++ devider ++ password ++ devider ++ login
writeRecord (Record name password Nothing) devider =
    name ++ devider ++ password


_createRecordFrom :: [String] -> Maybe Record
_createRecordFrom (name : password : []) =
    Just Record {
        name = name
      , password = password
      , mark = Nothing }
_createRecordFrom (name : password : mark : xs) =
    Just Record {
        name = name
      , password = password
      , mark = Just mark }
_createRecordFrom _ = Nothing
