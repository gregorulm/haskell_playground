{-
Gregor Ulm

This program explores Benford's Law:
http://en.wikipedia.org/wiki/Benford's_law

To quote from the Wikipedia article directly:
""
Benford's Law, also called the First-Digit Law, refers to the frequency
distribution of digits in many (but not all) real-life sources of data. In this
distribution, 1 occurs as the leading digit about 30% of the time, while larger
digits occur in that position less frequently: 9 as the first digit less than
5% of the time. Benford's Law also concerns the expected distribution for digits
beyond the first, which approach a uniform distribution.
""

The code below is based on the nifty assignment "Natural Prestidigitation"
http://nifty.stanford.edu/2006/wolfman-pretid/
http://nifty.stanford.edu/2006/wolfman-pretid/hw.html

The assignment, as well as the code, is rather straightforward. I did not
slavishly follow the assignment, though, which means that some of my
functions are implemented differently. I also skipped some of the busywork
of this assignment.

Specification:
- input: a txt file with one integer in each line
- output: the distribution of the most significant digits (decimal)

-}

import System.IO

processFile :: IO [String]
processFile = do
    contents <- readFile "numbers.txt"
    return $ words contents

readNumbers :: [String] -> [Int]
readNumbers = map read

-- extract the most significant digit of a number
getMSD :: Int -> Int
getMSD n = read $ [head (show n)]

tallyDigits :: [Int] -> [Int]
tallyDigits xs = tallyDigits' xs list
    where list = replicate 10 0
    
tallyDigits' :: [Int] -> [Int] -> [Int]
tallyDigits' []       res = res
tallyDigits' (pos:xs) res = tallyDigits' xs (updateList pos)
    where updateList pos = take (pos) res ++ [(res!!pos) + 1]
                            ++ drop (pos + 1) res

printResults :: [(Int,Int)] -> IO ()
printResults [] = return ()
printResults ((pos,count):xs) = do
    putStrLn $ show pos ++ "s: " ++ show count
    printResults xs

main :: IO ()
main = do
    input <- processFile
    let numbers               = readNumbers input
        mostSignificantDigits = map getMSD numbers
        counts                = tallyDigits mostSignificantDigits
        pairs                 = zip [0..9] counts
    --print mostSignificantDigits
    --print counts
    --return ()
    printResults pairs