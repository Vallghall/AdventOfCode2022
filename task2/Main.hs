module Main where


import System.Environment (getArgs)
import System.Directory (doesFileExist)

type Strategy = [(RPS, RPS)]

data RPS = Rock | Paper | Scissors
    deriving (Eq, Show)

main :: IO ()
main = do
    inputFileName <- head <$> getArgs
    strategy  <- readInput inputFileName
    strategy2 <- readInputP2 inputFileName
    print $ totalScore strategy
    print $ totalScore strategy2
    where
        totalScore :: Strategy -> Int
        totalScore = foldl (\acc x -> acc + scoreForRound x) 0

readRPS :: String -> RPS
readRPS str
    | str == "A" || str == "X" = Rock
    | str == "B" || str == "Y" = Paper
    | otherwise                = Scissors

readRPS2 :: String -> String -> (RPS, RPS)
readRPS2 "A" x
    | x == "X"  = (Rock, Scissors)
    | x == "Y"  = (Rock, Rock)
    | otherwise = (Rock, Paper)
readRPS2 "B" x
    | x == "X"  = (Paper,Rock)
    | x == "Y"  = (Paper, Paper)
    | otherwise = (Paper, Scissors)
readRPS2 "C" x
    | x == "X"  = (Scissors, Paper)
    | x == "Y"  = (Scissors, Scissors)
    | otherwise = (Scissors, Rock)

price :: RPS -> Int
price Rock      = 1
price Paper     = 2
price Scissors  = 3

scoreForRound :: (RPS, RPS) -> Int
scoreForRound rpc =
    case rpc of
        (Scissors, Rock)  -> 6 + points
        (Rock, Paper)     -> 6 + points
        (Paper, Scissors) -> 6 + points
        (a,b) | a == b    -> 3 + points
        (_,_)             -> 0 + points
    where points = price (snd rpc)


readInput :: FilePath -> IO Strategy
readInput fp = do
    exists <- doesFileExist fp
    if not exists
        then error "file not found"
        else map parseMoves . lines <$> readFile fp
    where
        parseMoves :: String -> (RPS, RPS)
        parseMoves s = do
            let x = take 1 s
                y = take 1 . drop 2 $ s
            (readRPS x, readRPS y)


readInputP2 :: FilePath -> IO Strategy
readInputP2 fp = do
    exists <- doesFileExist fp
    if not exists
        then error "file not found"
        else map parseMoves . lines <$> readFile fp
    where
        parseMoves :: String -> (RPS, RPS)
        parseMoves s = do
            let x = take 1 s
                y = take 1 . drop 2 $ s
            readRPS2 x y
