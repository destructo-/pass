module Services.Interaction (
    requestKeypass
  , requestPasswordFor
  , showHelp
  , alreadyExist
  , allDone
  , failedToFind
  , printMarkAndPutPass
  ) where

import System.IO (
    hFlush
  , hGetEcho
  , hSetEcho
  , stdin
  , stdout
  , hPutStr
  , hClose
  )
import Control.Exception (
    bracket_
  )

import System.Process ( callCommand )

import Domain.Commands (Command(..))
import Domain.Record (Record(..), Name, Password)


alreadyExist :: Name -> IO ()
alreadyExist name =
    putStrLn $ "this name [" ++ name ++ "] already exist"


allDone :: IO ()
allDone = putStrLn "all done!"


failedToFind :: Name -> IO ()
failedToFind name =
    putStrLn $ "failed to find record for name [" ++ name ++ "]"


printMarkAndPutPass :: Record -> IO ()
printMarkAndPutPass (Record _ pass (Just mark)) = do
    _ <- putStrLn mark
    _setClipboard pass
printMarkAndPutPass (Record _ pass Nothing) =
    _setClipboard pass


requestPasswordFor :: Name -> IO String
requestPasswordFor name = do
    putStr $ "[pass] enter password for [" ++ name ++ "]: "
    _hiddenInput


requestKeypass :: IO String
requestKeypass = do
    putStr "[pass] enter keypass: "
    _hiddenInput


_setClipboard :: Password -> IO ()
_setClipboard password =
    callCommand $ "echo \"" ++ password ++ "\" | xclip"


_hiddenInput :: IO String
_hiddenInput = do
    hFlush stdout
    keypass <- _withEcho False getLine
    putChar '\n'
    pure keypass


_withEcho :: Bool -> IO a -> IO a
_withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


showHelp :: IO ()
showHelp =
  putStrLn (
    "pass is a password manager.\n" ++
    "pass " ++ show Find ++ " [NAME] \t\t\t\t get stored password by [NAME].\n" ++
    "pass " ++ show Add ++ " [NAME] OPTION[-m[MARK]]  \t add new password by [NAME], and optional [MARK] like login for example\n" ++
    "pass " ++ show Delete ++ " [NAME] \t\t\t delete record with [NAME].\n" ++
    "pass " ++ show List ++ " \t\t\t\t show stored [NAME]s.\n" ++
    "pass " ++ show Update ++ " [NAME] OPTION[-m[MARK]] \t update exised record by [NAME], and optional [MARK]\n" ++
    "pass " ++ show Help ++ " \t\t\t\t show this help."
  )