module Main where

import System.Environment (
    getArgs
  )

import qualified Application


main :: IO ()
main = getArgs >>= Application.run