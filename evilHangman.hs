{-
Gregor Ulm

Evil Hangman

...not that playing hangman against an opponent that picks a random word from
a dictionary wasn't challenging enough!

This program is (roughly) based on an assignment originally given by
Keith Schwartz at Stanford:
http://nifty.stanford.edu/2011/schwarz-evil-hangman/
http://nifty.stanford.edu/2011/schwarz-evil-hangman/Evil_Hangman.pdf

One noteworthy difference is that the computer checks whether it can pick a word
to dodge your guess. Further, equivalence classes are based on the number of
missing letters.

Again, this program is relatively unoptimized. It simply gradually reduces
the list of words that match the game state. As it turns out, even
interpreted Haskell is fast enough.

This program seems to work fine, but was not extensively tested against
corner cases.
-}

import Data.Char
import Data.List
import System.IO
import System.Random

type Words    = [(Word, Pattern, Unknowns)]
type Word     = String
type Pattern  = String
type Unknowns = Int
type Guessed  = [Char]

processFile :: IO [Word]
processFile = do
    contents <- readFile "words.txt" 
--    putStrLn $ show $ maximum $ map length (words contents)  -- 21
--    putStrLn $ show $ minimum $ map length (words contents)  -- 2
    return $ words contents

pickword :: [String] -> IO String
pickword xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

newGuess :: [Char] -> IO [Char]
newGuess cs = do
    putStrLn "Guess a character!"
    c <- getChar
    putStrLn ""
    let c' = [toLower c]    
    return $ c' `union` cs

-- the game is won if the pattern contains no missing letters   
isSolved :: Pattern -> Bool
isSolved = not . elem '_'

-- find words with highest number of missing letters

getPattern :: Guessed -> Word -> Pattern
getPattern guessed = map (\c -> if c `elem` guessed then c else '_' )

getUnknowns :: Pattern -> Int
getUnknowns = sum . map (\x -> if x == '_' then 1 else 0)

updateWords :: Guessed -> Words -> Words
updateWords _ []                 = []
updateWords g ((w, known, _):ws) = (w, known', count') : updateWords g ws
    where known' = getPattern g w
          count' = getUnknowns known'
  
updatePattern :: Words -> Pattern
updatePattern [] = ""
updatePattern ((w, p, c):ws) = p 

largestCount :: Words -> Int
largestCount = maximum . map (\(_, _, count) -> count)

-- removes all words that don't contain the maximum of unknown letters
newList :: Int -> Words -> Words
newList maxCount = filter (\(_, _, count) -> count == maxCount)
    
printTallyMarks :: Int -> IO ()
printTallyMarks n = putStrLn $ replicate n '|'

getWord :: Words -> Word
getWord [] = error "this shouldn't happen"
getWord ws = (\(w,_,_) -> w) (head ws)

findPattern :: Pattern -> Words -> Bool
findPattern newPattern ws = newPattern `elem` [ p | (_, p, _) <- ws ]

removeNonPattern :: Pattern -> Words -> Words
removeNonPattern p = filter (\(_, x, _) -> x == p)

getNewPattern :: Pattern -> Words -> (Words, Pattern)
getNewPattern p ws = (matches, pattern)
    where maxWords = filter (\(_,_,x) -> x == maxCount) ws
          maxCount = largestCount ws
          -- possibly competing pattern; simply pick first one
          pattern  = head [ p | (_, p, _) <- maxWords]
          -- filter list to only include words with new pattern
          matches  = filter (\(_, p, _) -> p == pattern) maxWords

gameLoop :: [Char] -> Words -> Pattern -> Int -> IO ()
gameLoop cs wordList pattern n
    | isSolved pattern = putStrLn $ "Good job!\n" ++ pattern
    | n == 0           = putStrLn $ "Game over.\nThe word was (maybe) "
                                     ++ (getWord wordList) ++ "."         
    | otherwise    = do
        putStrLn "Remaining:"
        printTallyMarks n
        putStrLn pattern        
        cs' <- newGuess cs

        case cs' == cs of
            True  -> do putStrLn "Try again!"   -- illegal input
                        gameLoop cs wordList pattern n    
            -- this takes the case that the game is won into account, since
            -- pattern' /= ""
            False -> do
                let wordList'   = updateWords cs' wordList                    
                    -- is the old pattern still in wordList' ?
                    canTrick  = findPattern pattern wordList'
                    
                case canTrick of
                    -- old pattern in wordList:
                    True  -> do putStrLn "Too bad!"
                                let ws = removeNonPattern pattern wordList' 
                                gameLoop cs' ws pattern (n - 1)
                    False -> do putStrLn "Lucky you!\n"
                                let (ws, p) = getNewPattern pattern wordList'
                                gameLoop cs' ws p n    

main :: IO ()
main = do
    putStrLn "Evil Hangman with Tallymarks.\nYou're allowed to make 12 errors."
    dict        <- processFile
    -- let dict = ["abcd", "rrrr", "abrr", "rrxx", "ebcd"] -- testing
    wordLength  <- randomRIO (2, 21)   
    -- let wordLength = 4  -- testing
    let wordList  = filter (\x -> length x == wordLength) dict
        pattern   = replicate wordLength '_'
        wordList' = map (\x -> (x, pattern, wordLength)) wordList
    gameLoop [] wordList' pattern 12