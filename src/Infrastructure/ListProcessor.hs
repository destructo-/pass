module Infrastructure.ListProcessor (process) where


import qualified Config
import qualified Domain.Utils.ListUtils  as ListUtils
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import           Domain.Record (Record, Name, Mark, readRecord, createRecord, writeRecord)
import Domain.RecordsSeq (
    RecordsSeq
  , readRecordsSeq
  , findRecord
  , getNamesList )
import Data.Maybe (mapMaybe)


process :: IO ()
process = do
    keypass         <- Interaction.requestKeypass
    storedData      <- Repository.findStoredData Config.dataResource
    let decodedData =  Codec.decode storedData keypass
    let recordsSeq  =  readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    let namesList   =  getNamesList recordsSeq
    maybeName       <- _requestListElement namesList
    let maybeRecord = maybeName >>= (`findRecord` recordsSeq)
    maybe
      Interaction.allDone Interaction.printMarkAndPutPass maybeRecord


_requestListElement :: [(Int, Name)] -> IO (Maybe Name)
_requestListElement namesList = do
    _ <- Interaction.printNamesList namesList
    input <- getLine
    case input of
        "q" -> pure Nothing
        str -> case input `_maybeNamesElement` namesList of
            Just n -> pure $ Just n
            Nothing -> _requestListElement namesList


_maybeNamesElement :: String -> [(Int, Name)] -> Maybe Name
_maybeNamesElement input namesList =
    let filteredList = filter (\r -> show (fst r) == input) namesList
        names = map snd filteredList
    in  ListUtils.maybeHead names