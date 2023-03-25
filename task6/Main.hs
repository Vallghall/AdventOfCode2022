module Main where

import System.Environment (getArgs)
import Data.List (nub)

main :: IO ()
main = do
    filename <- head <$> getArgs
    buffer   <- readFile filename
    putStrLn $ "Ответ к первой части: " ++ show (getMarkerPosition buffer)
    putStrLn $ "Ответ ко второй части: " ++ show (getMsgPosition buffer)

getMarkerPosition :: String -> Int
getMarkerPosition = findPosition 4 4

getMsgPosition :: String -> Int
getMsgPosition = findPosition 14 14

findPosition :: Int -> Int -> String -> Int
findPosition piv n xs = 
    if isMarker (take piv xs)
        then n
        else findPosition piv (n + 1) (tail xs)
    where
        isMarker :: String -> Bool
        isMarker = (==piv) . length . nub