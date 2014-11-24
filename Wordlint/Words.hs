module Wordlint.Words where
--
-- This module contains types and functions for working with words and their
-- positions used in processing of file. A "Word" is a data structure
-- containing a word string; its position by word count, line, or percentage of
-- file used to calculate intervening distances; and its line and and column
-- positions used when returning data.
--
-- To create each word, four sets of functions each provide one element and
-- these are zipped together for each word in the file. Currently, a "word"
-- follows the basic prelude definition and therefore includes punctuation
-- marks. In a way, this is helpful to catch, for example, the repetition of
-- transition words such as "Furthermore,". Additionally, words are strictly
-- matched by their case and no "ignore-case" or similar option is yet
-- available. Future improvements will include providing options to handle
-- punctuation and case as well as providing a word blacklist.
--

import Data.List 

type Line = Int

type Column = Int

data Word a = Word 
    { lemma :: String
    ,position :: a
    ,line :: Line
    ,column :: Column } 

type Words a = [Word a] 

--Words are tested for equality and ordered by their lemma 
instance Eq (Word a)  where
    x == y = lemma x == lemma y

instance Ord (Word a) where
    compare x y = compare (lemma x) (lemma y)

--
-- Position
-- Create list of tuples containing lemma and word position
-- "Position" depends on type-of-check
-- For word and line checks, Position is an Int representing
-- the word count and line positions, respectively.
-- A percentage check returns a Position of type Double,
-- representing the Word's position as a percentage of
-- total words in the file.
-- 

class NumOps a where
    createPos :: String -> String -> [(String, a)]

instance NumOps Double where
    createPos text _ = createPercentPos text

instance NumOps Int where
    createPos = wordOrLine 

wordOrLine :: String -> String -> [(String, Int)]
wordOrLine s t = case t of
    "word" -> createWordPos s
    "line" -> createLinePos s
    _    -> createWordPos s

createWordPos :: String -> [(String, Int)]
createWordPos s  = zip (words s) [1..]

createLinePos :: String -> [(String, Int)]
createLinePos = createWordLinePos

createPercentPos :: String  -> [(String, Double)]
createPercentPos s =  getWordPercentPos wrdlst wrdln
  where wrdln = length $ words s
        wrdlst = createWordPos s

getWordPercentPos :: [(String, Int)] -> Int ->  [(String, Double)]
getWordPercentPos []  _ = []
getWordPercentPos (x:xs) y = divWordPercentPos x y : getWordPercentPos xs y

divWordPercentPos :: (String, Int) -> Int -> (String, Double)
divWordPercentPos (s,x) y = (s,p) 
  where xi = fromIntegral x
        yi = fromIntegral y
        p = (xi/yi)*100

--
-- Line coordinate
-- Create list of tuples containing lemma and line position

createWordLinePos :: String -> [(String, Int)]
createWordLinePos xs = setWordLines $ zip (getWordLines xs) [1..]

-- Makes lists of words by line, fed to the functions below
getWordLines :: String -> [[String]]
getWordLines xs = fmap words (lines xs)

setWordLine :: ([String], Int) -> [(String, Int)]
setWordLine (([],_)) = []
setWordLine ((x:xs,i)) = (x,i) : setWordLine (xs,i)

setWordLines :: [([String], Int)] -> [(String, Int)]
setWordLines [] = []
setWordLines (x:xs) = setWordLine x ++ setWordLines xs

--Column coordinate
--1) lines on file and zip infinite lists for each char. [[(Char,Int)]]
--2) compare first char of words from each inner [[String]] 
--   and add Int from [[(Char,Int)]] when Char == first Char of string

createWordColPos :: String -> [(String, Int)]
createWordColPos xs = setWordCols lin $ numWordCols lin
  where lin = lines xs

-- number columns in each line of file
numWordCols :: [String] -> [[(Char,Int)]]
numWordCols [[]] = [[]]
numWordCols [] = [[]]
numWordCols (x:xs) = zip x [1..] : numWordCols xs

-- call filter on lines
setWordCols :: [String] -> [[(Char,Int)]] -> [(String,Int)]
setWordCols [] [] = []
setWordCols (_:_) [] = []
setWordCols [] (_:_) = []
setWordCols (x:xs) (y:ys) = filtWordCols (words x) y ++ setWordCols xs ys

filtWordCols :: [String] -> [(Char,Int)] -> [(String,Int)]
filtWordCols [] _ = []
filtWordCols _ [] = []
filtWordCols w@(x:xs) c@(y:ys) = if (fst y == ' ') || (fst y /= head x)
                         then filtWordCols w ys
                         else (x,snd y) : filtWordCols xs (drop (length x) c )

--
-- Master functoion to create a list of words from a file.    
--
zipWords :: (NumOps a) => String -> String -> Words a
zipWords s t = zipWith4 Word (words s) (wordpos s t) linepos colpos
  where 
    linepos = snd . unzip $ createWordLinePos s
    colpos = snd . unzip $ createWordColPos s
    wordpos u v = snd . unzip $ createPos u v

--
-- Functions to operate on Words (lists of words).

-- Check a word against the minimum word length (wordlength Arguments in WordlintArgs)
isCheckWordLong :: (NumOps a) => Word a -> Int -> Bool
isCheckWordLong (Word w _ _ _) x = length w > x

-- Filter Words based on minimum word length
checkWordList :: (NumOps a) => Words a -> Int -> Words a
checkWordList [] _ = []
checkWordList (x:xs) i = if isCheckWordLong x i
                     then x : checkWordList xs i
                     else checkWordList xs i

-- Equality function checking for string match in different coordinate positions
-- Converts coordinates to tuples to ensure proper equality checking
checkWordEquality :: (NumOps a) => Word a -> Word a -> Bool
checkWordEquality (Word a _ b c) (Word x _ y z) = coord1 /= coord2 && a==x
  where coord1 = (b,c) 
        coord2 = (y,z)

-- Function to determine distance between two words
-- Uses Position of Word so it is type-of-search independent
checkWordDistance :: (Num a, NumOps a) => Word a -> Word a -> a
checkWordDistance (Word _ x _ _) (Word _ y _ _) = x - y

-- Filter all but matching pairs of words
filterMatchingWords :: (NumOps a) => Words a -> Words a
filterMatchingWords [] = []
filterMatchingWords xs = sort $ intersectBy checkWordEquality xs xs
