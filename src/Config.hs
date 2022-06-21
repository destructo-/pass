module Config (
    dataResource
) where

import Domain.Resource (Resource(..))
import Domain.Record (RecordDevider)


dataResource :: Resource
dataResource = Resource {
    relativeDir = ".config/pass"
  , fileName = "data" }


recordDevider :: RecordDevider
recordDevider = "|>"
