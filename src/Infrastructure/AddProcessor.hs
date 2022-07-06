module Infrastructure.AddProcessor (
    process
) where

import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import qualified Domain.Record           as Record
import qualified Services.RecordsSeq     as RecordsSeq


process :: Record.Name -> Maybe Record.Mark -> IO ()
process name maybeMark = do
    keypass                       <- Interaction.requestKeypass
    storedData                    <- Repository.findStoredData Config.dataResource
    let decodedData               =  Codec.decode storedData keypass
    recordsSeq                    <- RecordsSeq.readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    (maybeNewRec, updatedRecsSeq) <- _addIfPossible name maybeMark recordsSeq
    updatedData                   <- RecordsSeq.writeRecordsSeq updatedRecsSeq Config.lineDevider Config.recordDevider
    let encodedData               =  Codec.encode updatedData keypass
    _                             <- Repository.updateStoredData Config.dataResource encodedData
    case maybeNewRec of
        Nothing -> Interaction.alreadyExist name
        Just re -> Interaction.allDone


_addIfPossible :: Record.Name ->
                  Maybe Record.Mark ->
                  RecordsSeq.RecordsSeq ->
                  IO (Maybe Record.Record, RecordsSeq.RecordsSeq)
_addIfPossible name maybeMark records =
    case name `RecordsSeq.findRecord` records of
        Just re -> pure (Nothing, records)
        Nothing -> do
            recordPassword        <- Interaction.requestPasswordFor name
            let newRecord         =  Record.createRecord name recordPassword maybeMark
            let updatedRecordsSeq =  newRecord `RecordsSeq.addIfNotExist` records
            pure (Just newRecord, updatedRecordsSeq)