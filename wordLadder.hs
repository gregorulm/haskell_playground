{-

Gregor Ulm

This program finds the shortest "ladder" from one five-letter word to another,
where each subsequent word differs by exactly one letter. For instance, the
ladder from "crock" to "crown" is ["crock", "crook", "croon", "crown"].

The inspriation comes from this assignment description:
http://www.cs.duke.edu/csed/poop/ladder/

Boundary cases taken into account:
1) start word == target word is taken into account.
2) no ladder from start to target

The program runs rather slowly, so there is potential for improvement, for
instance by preprocessing the dictionary to turn into into an undirected
graph where the nodes contain all the words that differ from each other by
exactly one letter.

Sample run:
"cays to zigs"
["cays","bays","bags","zags","zigs"]

-}

import System.IO
import System.Random
import Data.List

processFile :: Int ->  IO [String]
processFile n = do
    contents <- readFile "words.txt"
    let xLetterWords = filter (\x -> length x == n) (words contents)
    --let fiveLetterWords = ["crock", "crook", "croon", "crown", "crowd"]
    return xLetterWords
    
pickWord :: [String] -> IO String
pickWord xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

getLastWords :: [[String]] -> [(String, [String])]
getLastWords xs = zip (map last xs) xs

findNext :: [[String]] -> [String] -> [[String]]
findNext ladders allWords = findNext' ladders allWords []

findNext' :: [[String]] -> [String] -> [[String]] -> [[String]]
findNext' []     _             res = res
findNext' (ladder:xs) allWords res = findNext' xs allWords (res ++ ladders)
    where nextWords = getNextWords (last ladder) allWords ladder
          ladders   = newLadders ladder nextWords

-- find all words, for each candidate "ladder", that differ from the preceding
-- word by exactly one letter; exlude words already in "ladder"
getNextWords :: String -> [String] -> [String] -> [String]
getNextWords word allWords ladder = candidates \\ ladder
    where candidates = filter (match4 word) allWords

match4 :: String -> String -> Bool
match4 a b = 1 == (sum $ map (\(x, y) -> if x /= y then 1 else 0) (zip a b))

newLadders :: [String] -> [String] -> [[String]]
newLadders ladder nextWords = map (\(x,y) -> x ++ [y]) pairs
    where ladders = replicate (length nextWords) ladder
          pairs   = zip ladders nextWords

loop :: String -> [[String]] -> [String] -> [String]
loop target ladders allWords
    | target `elem` (map fst currentList) =
        snd $ head $ filter (\(x, _) -> x == target) currentList 
    | otherwise = if ladders' == ladders
                  then error "no path from A to B"
                  else loop target ladders' allWords
        where currentList = getLastWords ladders
              ladders'    = findNext ladders allWords

main :: IO ()
main = do
    let wordLength = 4
    allWords <- processFile wordLength
--    let start = "crock"
--        target = "crown"
    start    <- pickWord allWords
    target   <- pickWord allWords
    print $ start ++ " to " ++ target
    let ladder = loop target [[start]] allWords
    print ladder