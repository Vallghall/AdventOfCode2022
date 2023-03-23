module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    inputFileName <- head <$> getArgs
    answer <- maximum . processInput <$> readInput inputFileName
    print answer

readInput :: FilePath -> IO [String]
readInput fp = do
    exists <- doesFileExist fp
    if not exists
        then error "file not found"
        else splitOn "\n\n" <$> readFile fp

processInput :: [String] -> [Int]
processInput = map $ foldl (\acc x -> acc + read x) 0 . lines
