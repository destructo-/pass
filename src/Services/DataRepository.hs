{-# LANGUAGE LambdaCase #-}

module Services.DataRepository (
    findStoredData
  , updateStoredData
) where

import System.Directory (
    createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory )
import System.IO ( readFile' )

import Domain.Resource (Resource(..), makeFullDirectory, makeFullPath)


_createParents :: Bool
_createParents = True


findStoredData :: Resource -> IO String
findStoredData resource = do
    dir   <- makeFullDirectory resource
    path  <- makeFullPath resource
    _     <- createDirectoryIfMissing _createParents dir
    _     <- _createFileIfMissing path
    readFile' path


updateStoredData :: Resource -> String -> IO ()
updateStoredData resource updatedData = do
    dir   <- makeFullDirectory resource
    path  <- makeFullPath resource
    _     <- createDirectoryIfMissing _createParents dir
    _     <- _createFileIfMissing path
    writeFile path updatedData


_createFileIfMissing :: FilePath -> IO ()
_createFileIfMissing pathToFile =
    doesFileExist pathToFile >>= \case
        True -> pure ()
        False -> writeFile pathToFile ""


