module Infrastructure.AddProcessor (
    process
) where

import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import           Domain.Record (Name, Mark, readRecord, createRecord, writeRecord)
import           Domain.RecordsSeq (readRecordsSeq, writeRecordsSeq, addIfNotExist)


process :: Name -> Maybe Mark -> IO ()
process name maybeMark = do
    keypass               <- Interaction.requestKeypass
    storedData            <- Repository.findStoredData Config.dataResource
    let decodedData       =  Codec.decode storedData keypass
    let recordsSeq        =  readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    recordPassword        <- Interaction.requestPasswordFor name
    let newRecord         =  createRecord name recordPassword maybeMark
    let updatedRecordsSeq =  newRecord `addIfNotExist` recordsSeq
    let updatedData       =  writeRecordsSeq updatedRecordsSeq Config.lineDevider Config.recordDevider
    let encodedData       =  Codec.encode updatedData keypass
    _                     <- Repository.updateStoredData Config.dataResource encodedData
    if newRecord `elem` recordsSeq
        then Interaction.alreadyExist name
        else Interaction.allDone