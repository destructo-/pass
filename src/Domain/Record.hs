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


type RecordDevider = String

type Name = String
type Password = String
type Mark = String

data Record = Record {
    name :: Name
  , password :: Password
  , mark :: Maybe Mark }


createRecord :: Name -> Password -> Maybe Mark -> Record
createRecord name pass mark =
    Record {
        name = name
      , password = pass
      , mark = mark
    }


readRecord :: String -> RecordDevider -> Maybe Record
readRecord str devider = _createRecord $ splitOn devider str


writeRecord :: Record -> RecordDevider -> String
writeRecord (Record name password (Just login)) devider =
    name ++ devider ++ password ++ login
writeRecord (Record name password Nothing) devider =
    name ++ devider ++ password


_createRecord :: [String] -> Maybe Record
_createRecord (name : password : [[]]) =
    Just Record {
        name = name
      , password = password
      , mark = Nothing }
_createRecord (name : password : login : xs) =
    Just Record {
        name = name
      , password = password
      , mark = Just login }
_createRecord _ = Nothing