module Application (run) where

import qualified Infrastructure.AddProcessor    as AddProcessor
import qualified Infrastructure.HelpProcessor   as HelpProcessor
import qualified Infrastructure.ListProcessor   as ListProcessor
import qualified Infrastructure.DeleteProcessor as DeleteProcessor
import qualified Infrastructure.FindProcessor   as FindProcessor
import qualified Infrastructure.UpdateProcessor as UpdateProcessor
import qualified Domain.Utils.ListUtils         as ListUtils
import           Domain.Commands (Command (..), fromMaybeString)


run :: [String] -> IO ()
run args = do
    let command = (fromMaybeString $ ListUtils.maybeHead args) :: Command
    let additioanlCommandArgs = _extractAdditionalArgs command args
    _runOnProcessor command additioanlCommandArgs


_runOnProcessor :: Command -> [String] -> IO ()
_runOnProcessor Help   _                   = HelpProcessor.process
_runOnProcessor Add    (name:"-m":mark:xs) = AddProcessor.process name (Just mark)
_runOnProcessor Add    (name:xs)           = AddProcessor.process name Nothing
_runOnProcessor Delete (name:xs)           = DeleteProcessor.process name
_runOnProcessor Update (name:"-m":mark:xs) = UpdateProcessor.process name (Just mark)
_runOnProcessor Update (name:xs)           = UpdateProcessor.process name Nothing
_runOnProcessor List   _                   = ListProcessor.process
_runOnProcessor Find   (name:xs)           = FindProcessor.process name
_runOnProcessor _      []                  = HelpProcessor.process


_extractAdditionalArgs :: Command -> [String] -> [String]
_extractAdditionalArgs Find args = args
_extractAdditionalArgs _ args    = ListUtils.safeTail args