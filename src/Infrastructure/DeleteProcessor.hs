module Infrastructure.DeleteProcessor (process) where


import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import           Domain.Record (Name, Mark, readRecord, createRecord, writeRecord)
import           Domain.RecordsSeq (readRecordsSeq, writeRecordsSeq, addIfNotExist, removeIfExist)


process :: Name -> IO ()
process name = do
    keypass               <- Interaction.requestKeypass
    storedData            <- Repository.findStoredData Config.dataResource
    let decodedData       =  Codec.decode storedData keypass
    let recordsSeq        =  readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    let updatedRecordsSeq =  name `removeIfExist` recordsSeq
    let updatedData       =  writeRecordsSeq updatedRecordsSeq Config.lineDevider Config.recordDevider
    let encodedData       =  Codec.encode updatedData keypass
    _                     <- Repository.updateStoredData Config.dataResource encodedData
    if length recordsSeq == length updatedRecordsSeq
        then Interaction.failedToFind name
        else Interaction.allDone