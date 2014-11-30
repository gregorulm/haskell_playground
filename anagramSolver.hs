{-

Gregor Ulm

Anagram Solver:
Enter a word to receive a list of anagrams found in a dictionary.

Sample run:

Enter a word:
creative
["reactive"]

-}

import System.IO
import Data.List


processFile :: IO [String]
processFile = do
    contents <- readFile "words.txt"    
    return $ words contents
    
-- strings are lists of characters, e.g. 'sort "testing"' yields "eginstt"
preprocessDict :: [String] -> [(String, String)]
preprocessDict = map (\x -> (x, sort x))

getWord :: IO String
getWord = do x <- getLine
             return x

getAnagrams :: String -> [(String, String)] -> [String]
getAnagrams letters xs = map fst filtered
    where filtered = filter (\(_, x) -> x == letters) xs

printList :: [String] -> IO ()
printList = undefined

main :: IO ()
main = do
    allWords <- processFile   
    putStrLn "Enter a word:" 
    phrase   <- getWord
    let pairs  = preprocessDict allWords
        result = (getAnagrams (sort phrase) pairs) \\ [phrase]
    print result