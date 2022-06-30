module Infrastructure.UpdateProcessor (process) where


import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import           Domain.Record (Name, Mark, readRecord, createRecord, writeRecord, Record (name))
import Domain.RecordsSeq (
    RecordsSeq
  , readRecordsSeq
  , writeRecordsSeq
  , addIfNotExist
  , findRecord
  , removeIfExist)


process :: Name -> Maybe Mark -> IO ()
process name maybeMark = do
    keypass                       <- Interaction.requestKeypass
    storedData                    <- Repository.findStoredData Config.dataResource
    let decodedData               =  Codec.decode storedData keypass
    recordsSeq                    <-  readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    (maybeNewRec, updatedRecsSeq) <- _findAndUpdate name maybeMark recordsSeq
    updatedData                   <-  writeRecordsSeq updatedRecsSeq Config.lineDevider Config.recordDevider
    let encodedData               =  Codec.encode updatedData keypass
    _                             <- Repository.updateStoredData Config.dataResource encodedData
    case maybeNewRec of
        Nothing -> Interaction.failedToFind name
        Just re -> Interaction.allDone


_findAndUpdate :: Name -> Maybe Mark -> RecordsSeq -> IO (Maybe Record, RecordsSeq)
_findAndUpdate name maybeMark recordsSeq =
    case name `findRecord` recordsSeq of
        Nothing -> pure (Nothing, recordsSeq)
        Just re -> do
            recordPassword        <- Interaction.requestPasswordFor name
            let newRecord         =  createRecord name recordPassword maybeMark
            let cleanRecordSeq    = name `removeIfExist` recordsSeq
            let updatedRecordsSeq =  newRecord `addIfNotExist` cleanRecordSeq
            pure (Just newRecord, updatedRecordsSeq)