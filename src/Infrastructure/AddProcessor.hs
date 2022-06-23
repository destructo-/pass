module Infrastructure.AddProcessor (
    process
) where

import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import           Domain.Record (Record, Name, Mark, readRecord, createRecord, writeRecord)
import Domain.RecordsSeq (
    RecordsSeq
  , readRecordsSeq
  , writeRecordsSeq
  , addIfNotExist
  , findRecord)


process :: Name -> Maybe Mark -> IO ()
process name maybeMark = do
    keypass                       <- Interaction.requestKeypass
    storedData                    <- Repository.findStoredData Config.dataResource
    let decodedData               =  Codec.decode storedData keypass
    let recordsSeq                =  readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    (maybeNewRec, updatedRecsSeq) <- _addIfPossible name maybeMark recordsSeq
    let updatedData               =  writeRecordsSeq updatedRecsSeq Config.lineDevider Config.recordDevider
    let encodedData               =  Codec.encode updatedData keypass
    _                             <- Repository.updateStoredData Config.dataResource encodedData
    case maybeNewRec of
        Nothing -> Interaction.alreadyExist name
        Just re -> Interaction.allDone


_addIfPossible :: Name -> Maybe Mark -> RecordsSeq -> IO (Maybe Record, RecordsSeq)
_addIfPossible name maybeMark recordsSeq =
    case name `findRecord` recordsSeq of
        Just re -> pure (Nothing, recordsSeq)
        Nothing -> do
            recordPassword        <- Interaction.requestPasswordFor name
            let newRecord         =  createRecord name recordPassword maybeMark
            let updatedRecordsSeq =  newRecord `addIfNotExist` recordsSeq
            pure (Just newRecord, updatedRecordsSeq)