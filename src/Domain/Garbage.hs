module Domain.Garbage (generateGarbageStrings) where

import qualified System.Random as Random


type Min = Int
type Max = Int
type Range = (Min, Max)


_recordLength :: ([Int] -> Int) -> [String] -> Int
_recordLength f strs = f $ map length strs


generateGarbageStrings :: Int -> Range -> IO [String]
generateGarbageStrings 0 range                = pure []
generateGarbageStrings count (minStr, maxStr) = do
    generator <- Random.getStdGen
    let stringsLengths = take count $ Random.randomRs (minStr, maxStr) generator
    let randomStr  = Random.randomRs ('a', 'z') generator
    pure $ _splitString stringsLengths randomStr


_splitString :: [Int] -> String -> [String]
_splitString [] str     = []
_splitString (x:xs) str = take x str : _splitString xs (drop x str)