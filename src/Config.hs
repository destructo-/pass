module Config (
    dataResource
  , recordDevider
  , lineDevider
) where

import Domain.Resource (Resource(..))
import Domain.Record (RecordDevider)
import Services.RecordsSeq (LineDevider)


dataResource :: Resource
dataResource = Resource {
    relativeDir = ".config/pass"
  , fileName = "data" }


recordDevider :: RecordDevider
recordDevider = "||"


lineDevider :: LineDevider
lineDevider = "\n"