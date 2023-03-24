module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)
import Data.List (isSubsequenceOf, intersect)

type Interval = (Int, Int)
type AssignmentList = [(Interval, Interval)]

main :: IO ()
main = do
    filename <- head <$> getArgs
    aList    <- readInput filename
    putStrLn $ "Первая часть: " ++ show (totalOverlaps countFullyContain aList)
    putStrLn $ "Вторая часть: " ++ show (totalOverlaps countAll aList)
    where
        totalOverlaps :: ((Interval, Interval) -> Int) -> AssignmentList -> Int
        totalOverlaps f = foldl (\acc x -> acc + f x) 0

        countFullyContain :: (Interval, Interval) -> Int
        countFullyContain (lhs, rhs)
            | isSubsequenceOf (makeSet lhs) (makeSet rhs) = 1
            | isSubsequenceOf (makeSet rhs) (makeSet lhs) = 1
            | otherwise                                   = 0

        countAll :: (Interval, Interval) -> Int
        countAll (lhs, rhs) =
            case makeSet lhs `intersect` makeSet rhs of
                []  -> 0
                _   -> 1


        makeSet :: Interval -> [Int]
        makeSet (a,b) = [a..b]

readInput :: FilePath -> IO AssignmentList
readInput fp = do
    exists <- doesFileExist fp
    if not exists
        then error "file not found"
        else map toIntervalPair . lines <$> readFile fp
    where
        toIntervalPair :: String -> (Interval, Interval)
        toIntervalPair xs = do 
            let halves = splitOn "," xs
                lhs    = head halves
                rhs    = halves !! 1
            (divideOnMinus lhs,  divideOnMinus rhs)

        divideOnMinus :: String -> Interval
        divideOnMinus xs = do
            let halves = splitOn "-" xs
                lhs    = head halves
                rhs    = halves !! 1
            (read lhs, read rhs)


    
