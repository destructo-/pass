module Domain.Resource (
    Resource(..)
  , makeFullPath
  , makeFullDirectory
) where

import System.Directory (getHomeDirectory)


data Resource = Resource {
    relativeDir :: String
  , fileName :: String }


_makeRelativePath :: Resource -> FilePath
_makeRelativePath (Resource dir file) = dir ++ "/" ++ file


makeFullPath :: Resource -> IO FilePath
makeFullPath resource =
    getHomeDirectory >>= \homeDir ->
        pure $ homeDir ++ "/" ++ _makeRelativePath resource


makeFullDirectory :: Resource -> IO FilePath
makeFullDirectory (Resource dir _) =
    getHomeDirectory >>= \homeDir ->
        pure $ homeDir ++ "/" ++ dir
