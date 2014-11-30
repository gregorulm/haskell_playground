{-
Gregor Ulm

This program dramatially increases your odds of beating "hangman.hs". There are
several possible optimizations, for instance preprocessing the resulting
dict :: [String] by creating equivalence classes based on length. As it is,
this program is clearly "fast enough", though, as it returns matches
instantly.

Sample run:

Enter the known letters; indicate unknown ones with '_'.
__r__e__a
cerebella
paramenta

-}

import Data.Char

processFile :: IO [String]
processFile = do
    contents <- readFile "words.txt"
    return $ words contents
    
-- input is supposed to be [a-zA-Z_]*  
getInput :: IO String
getInput = do
    input <- getLine
    return $ map toLower input
    
getMatches :: String -> [String] -> [String]
getMatches target = filter (isMatch target)

isMatch :: String -> String -> Bool
isMatch a b
    | length a /= length b = False
    | otherwise = and $ map (\(a,b) -> a == b || a == '_' ) pairs
        where pairs = zip a b
{-
isMatch (a:as) (b:bs)
     | length (a:as) /= length (b:bs) = False
     | (a /= b) && (a /= '_')         = False
     | otherwise                      = isMatch as bs 
isMatch [] [] = True
-}

printMatches :: [String] -> IO ()
printMatches []     = putStr ""
printMatches (x:xs) = do putStrLn x
                         printMatches xs

-- check that input string is [a-z_]*, and at least of length 1
checkInput :: String -> Bool
checkInput xs = not (null xs) && all (\x -> x `elem` chars) xs
    where chars = '_' : ['a'..'z']

loop :: [String] -> IO ()
loop dict = do
    putStrLn "Enter the known letters; indicate unknown ones with '_'."
    input <- getInput
    case checkInput input of
        True -> do let list = getMatches input dict
                   printMatches list
                   loop dict
        False -> loop dict

main :: IO ()
main = do
    dict <- processFile
    loop dict