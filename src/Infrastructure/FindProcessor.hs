module Infrastructure.FindProcessor (process) where

import qualified Config
import qualified Services.DataRepository as Repository
import qualified Services.Interaction    as Interaction
import qualified Services.Codec          as Codec
import           Domain.Record (Name)
import           Domain.RecordsSeq (readRecordsSeq, findRecord)
import qualified Domain.RecordsSeq as Interaction



process :: Name -> IO ()
process name = do
    keypass            <- Interaction.requestKeypass
    storedData         <- Repository.findStoredData Config.dataResource
    let decodedData    =  Codec.decode storedData keypass
    recordsSeq     <- readRecordsSeq decodedData Config.lineDevider Config.recordDevider
    let searchedRecord =  name `findRecord` recordsSeq
    case searchedRecord of
        Just record -> Interaction.printMarkAndPutPass record
        Nothing     -> Interaction.failedToFind name
