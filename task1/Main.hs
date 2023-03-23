module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = do
    inputFileName <- head <$> getArgs
    calories <- processInput <$> readInput inputFileName
    let max'    = maximum calories
        top3sum = sum . take 3 . reverse . sort $ calories
    putStrLn $ "Первая часть: " ++ show max'
    putStrLn $ "Вторая часть: " ++ show top3sum

readInput :: FilePath -> IO [String]
readInput fp = do
    exists <- doesFileExist fp
    if not exists
        then error "file not found"
        else splitOn "\n\n" <$> readFile fp

processInput :: [String] -> [Int]
processInput = map $ foldl (\acc x -> acc + read x) 0 . lines
