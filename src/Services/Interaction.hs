module Services.Interaction (
    requestKeypass
  , hiddenRequest
  , showHelp
  ) where

import System.IO (
    hFlush
  , hGetEcho
  , hSetEcho
  , stdin
  , stdout
  )
import Control.Exception (
    bracket_
  )
import Domain.Commands (Command(..))


hiddenRequest :: String -> IO String
hiddenRequest name = do
    putStrLn $ "[pass] enter password for [" ++ name ++ "]: "
    _hiddenInput


requestKeypass :: IO String
requestKeypass = do
    putStr "[pass] enter keypass: "
    _hiddenInput


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
  putStr (
    "\tpass is a password manager.\n" ++
    "\tpass " ++ show Find ++ " [NAME] \t\t\t\t get stored password by [NAME].\n" ++
    "\tpass " ++ show Add ++ " [NAME] OPTION[-m[MARK]]  \t add new password or update exist password by [NAME], and optional [MARK] like login for example\n" ++
    "\tpass " ++ show Delete ++ " [NAME] \t\t\t delete record with [NAME].\n" ++
    "\tpass " ++ show List ++ " \t\t\t\t show stored [NAME]s.\n" ++
    "\tpass " ++ show Help ++ " \t\t\t\t show this help."
  )