module Main where

import System.Environment (
    getArgs
  )

import qualified Config
import qualified AddProcessor
import qualified HelpProcessor
import qualified Services.DataRepository  as DataRepository
import qualified Services.Interaction     as Interaction
import qualified Domain.Utils.ListUtils   as ListUtils
import           Domain.Commands (Command (..), fromMaybeString)


main :: IO ()
main = do
    args <- getArgs
    let command = (fromMaybeString $ ListUtils.maybeHead args) :: Command
    _ <- runOnProcessor command $ ListUtils.safeTail args

    keypass <- Interaction.requestKeypass
    _ <- putStrLn keypass

    _ <- Interaction.showHelp

    _ <- DataRepository.findStoredData Config.dataResource

    storedData <- DataRepository.findStoredData Config.dataResource
    putStrLn storedData


runOnProcessor :: Command -> [String] -> IO ()
runOnProcessor Help   _                     = HelpProcessor.process
runOnProcessor _      []                    = HelpProcessor.process
runOnProcessor Add    (name:"-m":mark:xs)   = AddProcessor.process name (Just mark)
runOnProcessor Add    (name:xs)             = AddProcessor.process name Nothing
runOnProcessor Delete (name:xs)             = putStrLn "DELETE"
runOnProcessor List   (name:xs)             = putStrLn "LIST"
runOnProcessor Find   (name:xs)             = putStrLn "FIND"