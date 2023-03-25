module Main where

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)
import Stack
import Data.List (transpose)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    path               <- head <$> getArgs
    (stacks, commands) <- readInput path
    let stacks'  = foldl (exec reverse) stacks (map words commands)
        stacks'' = foldl (exec id) stacks (map words commands)
    print $ mapMaybe peek stacks'
    print $ mapMaybe peek stacks''
    where
        exec :: (String -> String) -> [Stack Char] -> [String] -> [Stack Char]
        exec pick acc [_,t,_,from,_,to] = do
            let n     = read t
                to'   = read to - 1
                from' = read from - 1

                src   = acc !! from'
                dest  = acc !! to'
                
                (Just crates, src') = popN n src
                dest' = pushN (pick crates) dest

                acc' = setAtIndex to' dest' acc
            setAtIndex from' src' acc'

readInput :: FilePath -> IO ([Stack Char], [String])
readInput fp = do
    exists <- doesFileExist fp
    let input = if not exists
            then error "file not found"
            else pair . splitOn "\n\n" <$> readFile fp
    (crates, commands) <- input

    return (parseCrates crates, lines commands)
    where
        pair :: [a] -> (a,a)
        pair [a,b] = (a,b)
        pair []    = error "cannot make a pair"

parseCrates :: String -> [Stack Char]
parseCrates = loadCrates . map strip . transpose . init . lines
    where
        strip = filter (\x -> x /= '[' && x /= ']' && x /= ' ')
    
loadCrates :: [String] -> [Stack Char]
loadCrates =  map Stack . skipEmpty
    where skipEmpty = filter (/="")

setAtIndex :: Int -> a -> [a] -> [a]
setAtIndex i x xs = take i xs ++ [x] ++ drop (i+1) xs


