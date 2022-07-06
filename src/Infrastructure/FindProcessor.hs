module Infrastructure.FindProcessor (process) where

import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import qualified Domain.Record           as Records
import qualified Services.RecordsSeq     as RecordsSeq


process :: Records.Name -> IO ()
process name = do
    keypass            <- Interaction.requestKeypass
    storedData         <- Repository.findStoredData Config.dataResource
    let decodedData    =  Codec.decode storedData keypass
    recordsSeq         <- RecordsSeq.readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    let searchedRecord =  name `RecordsSeq.findRecord` recordsSeq
    case searchedRecord of
        Just record -> Interaction.printMarkAndPutPass record
        Nothing     -> Interaction.failedToFind name
