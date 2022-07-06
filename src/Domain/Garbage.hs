module Domain.Garbage (
    generateGarbageStrings
  , garbageStringsAvailableCount
  , randomMerge
) where

import qualified System.Random as Random


type Min = Int
type Max = Int
type Range = (Min, Max)


generateGarbageStrings :: Int -> Range -> IO [String]
generateGarbageStrings 0 range                = pure []
generateGarbageStrings count (minStr, maxStr) = do
    generator <- Random.getStdGen
    let stringsLengths = take count $ Random.randomRs (minStr, maxStr) generator
    let randomStr  = Random.randomRs ('a', 'z') generator
    pure $ _splitString stringsLengths randomStr


garbageStringsAvailableCount :: Int -> IO Int
garbageStringsAvailableCount stringsCount = do
    let minAvalableCount = stringsCount `div` 3
    fst . Random.randomR (minAvalableCount, stringsCount) <$> Random.getStdGen


randomMerge :: [String] -> [String] -> IO [String]
randomMerge first second = do
    generator <- Random.getStdGen
    let allLength = length first + length second
    let randomNumbers = take allLength $ Random.randomRs (0 :: Int, 100 :: Int) generator
    pure $ _merge randomNumbers first second


_merge :: [Int] -> [String] -> [String] -> [String]
_merge _ [] [] = []
_merge [] (first:ys) second = first : _merge [] ys second
_merge _ (first:ys) [] = first : _merge [] ys []
_merge _ [] (second:zs) = second : _merge [] [] zs
_merge (count:xs) allFirst@(first:ys) allSecond@(second:zs)
    | isEven    = first : _merge xs ys allSecond
    | otherwise = second : _merge xs allFirst zs
    where isEven = even count


_splitString :: [Int] -> String -> [String]
_splitString [] str     = []
_splitString (x:xs) str = take x str : _splitString xs (drop x str)