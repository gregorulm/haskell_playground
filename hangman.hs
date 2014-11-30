{-
Gregor Ulm
Simple "Hangman game", requires "words.txt".
-}

import Data.Char
import Data.List
import System.IO
import System.Random

processFile :: IO [String]
processFile = do
    contents <- readFile "words.txt"
    return $ words contents

pickword :: [String] -> IO String
pickword xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

output :: [Char] -> String -> String
output cs = map (\ c -> if c `elem` cs then c else '_' )

printWord :: [Char] -> String -> IO ()
printWord guessed target = putStr $ output guessed target

newGuess :: [Char] -> IO [Char]
newGuess cs = do
    putStrLn "Guess a character!"
    c <- getChar
    putStrLn ""
    let c' = [toLower c]    
    return $ c' `union` cs

printTallyMarks :: Int -> IO ()
printTallyMarks n = putStrLn $ replicate n '|'

isSolved :: [Char] -> String -> Bool
isSolved cs target = output cs target == target

gameLoop :: [Char] -> String -> Int -> IO ()
gameLoop cs target n
    | isSolved cs target = putStrLn "Good job!"
    | n == 0 = putStrLn $ "Game over.\nThe word was " ++ target ++ "."          
    | otherwise    = do
        putStrLn "Remaining:"
        printTallyMarks n
        
        let oldState = output cs target
        putStrLn $ oldState ++ "\n"
        
        cs'      <- newGuess cs
        let newState = output cs' target
        
        case cs' == cs of
            True  -> do putStrLn "Try again!"   -- illegal input
                        gameLoop cs target n
            False -> case oldState == newState of
                True  -> do putStrLn "Too bad!" -- unsuccessful guess 
                            gameLoop cs' target (n - 1)
                False -> do putStrLn "Lucky you!"
                            gameLoop cs' target n

main :: IO ()
main = do
    putStrLn "Hangman with Tallymarks.\nYou're allowed to make 9 errors."
    dict <- processFile
    target <- pickword dict
    gameLoop [] target 9