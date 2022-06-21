module AddProcessor (
    process
) where

import Data.List.NonEmpty (NonEmpty)

import Domain.Record (Name, Mark)


process :: Name -> Maybe Mark -> IO ()
process name args = putStrLn "add"


