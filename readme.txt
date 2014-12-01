Haskell Playground
Gregor Ulm

last update: 2014-12-01

This directory contains a number of short and possibly interesting programs
I've writen in Haskell.

- anagramSolver.hs
  Enter a word to get a list of anagrams.

- anagramSolverMulti.hs
  Get anagrams of multiple words; contains various optimzations.

- benfordsLaw.hs
  Illustrates Benford's Law, i.e. the observation that in numerical data the
  digit 1 as a leading digit is overrepresented.
  
- hangman.hs
  A straightforward, i.e. non-ASCII art implementation of the hangman game.
  
- hangmanReverse.hs
  Takes an input like _a__gm__ and outputs all matches, based on a dictionary.

- evilHangman.hs
  Turns hangman into a more devious version that tries dodging your guesses as
  you play.
  
- wordLadder.hs
  A fairly slow program that finds the shortest "ladder" from one word to
  another, for instance ["fane","fare","fard","nard"].