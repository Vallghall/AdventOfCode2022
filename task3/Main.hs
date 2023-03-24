module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List (intersect)
import Data.Char (ord, isLower)

main :: IO ()
main = do
    filename <- head <$> getArgs
    bags <- readInput filename
    let answer1 = processInput bags
        answer2 = parseGroups bags 0

    putStrLn $ "First part:\t" ++ show answer1
    putStrLn $ "Second part:\t" ++ show answer2
    where
        processInput :: [String] -> Int
        processInput = foldl (\acc x -> acc + calcIntersectPriority x) 0

readInput :: FilePath -> IO [String]
readInput fp = do
    exists <- doesFileExist fp
    if not exists
        then error "file not found"
        else lines <$> readFile fp

calcIntersectPriority :: String -> Int
calcIntersectPriority s = do
    let (lh, rh) = splitAt (length s `div` 2) s
    itemPriority . head $ intersect lh rh

itemPriority :: Char -> Int
itemPriority ch
    | isLower ch = ord ch - lowerSubtractor
    | otherwise  = ord ch - upperSubtractor
    where
        lowerSubtractor = ord 'a' - 1
        upperSubtractor = ord 'A' - 27

parseGroups :: [String] -> Int -> Int
parseGroups [] n = n
parseGroups xs n = parseGroups (drop 3 xs) ((n+) . groupBadgePriority $ take 3 xs)

groupBadgePriority :: [String] -> Int
groupBadgePriority [as, bs, cs] = itemPriority . head $ as `intersect` bs `intersect` cs