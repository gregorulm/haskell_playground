{-

Gregor Ulm

Anagram Solver for multiple inputs:
Enter several words to receive a list of anagrams found in a dictionary.

Sample run:

Enter a word. Type '0' to proceed to lookup.
xx
Enter a word. Type '0' to proceed to lookup.
abc
Enter a word. Type '0' to proceed to lookup.
gregor
Enter a word. Type '0' to proceed to lookup.
creative
Enter a word. Type '0' to proceed to lookup.
reactive
Enter a word. Type '0' to proceed to lookup.
0

xx
[]
==========
abc
["cab"]
==========
gregor
["gorger"]
==========
creative
["reactive"]
==========
reactive
["creative"]
==========
-}

import System.IO
import Data.List
import Data.Map as M hiding (map, (\\), filter, foldr)
import Data.Maybe

processFile :: IO [String]
processFile = do
    contents <- readFile "words.txt"    
    return $ words contents

-- this and the next step obviously take some time
preprocessDict :: [String] -> [(String, String)]
preprocessDict = map (\x -> (sort x, x))
    
storeDict :: M.Map String String -> [(String, String)] -> M.Map String String
storeDict kv []              = kv
storeDict kv ((sig,word):xs) = storeDict kv' xs
    where kv' = insertWithKey f sig word kv
          f key newVal oldVal = newVal ++ "|" ++ oldVal
             -- (show key) ++ ":" ++ newVal ++ "|" ++ oldVal
-- note regarding M.fromList: if there are multiple values for the same key,
-- then only the last one is retained, making it unforunately unsuitable for
-- this program

getWord :: IO String
getWord = do putStrLn "Enter a word. Type '0' to proceed to lookup."
             getLine

getWords :: [String] -> IO [String]
getWords xs = do
    inp <- getWord
    if inp == "0"
    then return xs
    else getWords (inp:xs)

-- sort all input phrases
sortPhrases :: [String] -> [String]
sortPhrases = map sort

getAnagram :: M.Map String String -> String -> String
getAnagram kv k = fromMaybe "" (M.lookup k kv)
{-    
    case M.lookup k kv of
      Nothing  -> ""
      Just val -> val
-}

getAnagrams :: M.Map String String -> [String] -> [String]
getAnagrams kv = map (getAnagram kv)

printResults :: [(String, String)] -> IO ()
printResults [] = return ()
printResults ((phrase, anagrams):xs) = do
    putStrLn phrase
    print $ splitVals anagrams \\ [phrase]
    putStrLn "=========="
    printResults xs

-- e.g. "reactive|creative" -> ["reactive", "creative"]
splitVals :: String -> [String]
splitVals = words . map (\c -> if c == '|' then ' ' else c)

main :: IO ()
main = do
    allWords    <- processFile
    allPhrases  <- getWords []
    let kv            = storeDict M.empty $ preprocessDict allWords
        sortedPhrases = sortPhrases allPhrases  
        results = getAnagrams kv sortedPhrases
    putStrLn ""
    printResults $ reverse $ zip allPhrases results