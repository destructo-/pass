module Infrastructure.DeleteProcessor (process) where


import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import qualified Domain.Record           as Record
import qualified Services.RecordsSeq     as RecordsSeq


process :: Record.Name -> IO ()
process name = do
    keypass               <- Interaction.requestKeypass
    storedData            <- Repository.findStoredData Config.dataResource
    let decodedData       =  Codec.decode storedData keypass
    recordsSeq            <- RecordsSeq.readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    let updatedRecordsSeq =  name `RecordsSeq.removeIfExist` recordsSeq
    updatedData           <- RecordsSeq.writeRecordsSeq updatedRecordsSeq Config.lineDevider Config.recordDevider
    let encodedData       =  Codec.encode updatedData keypass
    _                     <- Repository.updateStoredData Config.dataResource encodedData
    if length recordsSeq == length updatedRecordsSeq
        then Interaction.failedToFind name
        else Interaction.allDone