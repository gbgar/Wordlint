module WordcheckWords where
import Data.List
--
-- This module contains types and functions for working with words and their
-- positions used in processing of file. A "Word" is a data structure
-- containing a word string; its position by word count, line, or percentage of
-- file used to calculate intervening distances; and its line and and column
-- positions used when returning data.
--
-- To create each word, four sets of functions each provide one element and
-- these are zipped together for each word in the file. Currently, "word" is
-- simply the basic prelude definition and therefore includes punctuation
-- marks. In a way, this is helpful to catch, for example, the repetition of
-- transition words such as "Furthermore,". Additionally, words are strictly
-- matched by their case and no "ignore-case" or similar option is yet
-- available. Future improvements will therefore include providing options to
-- handle punctuation and case.
--
type Position = Int

type Line = Int

type Column = Int

data Word = Word 
    { lemma :: String
    ,position :: Position
    ,line :: Line
    ,column :: Column
    } deriving (Show)

type Words = [Word]

instance Eq Word where
    x == y = (lemma x) == (lemma y)

instance Ord Word where
    compare x y = compare (lemma x) (lemma y)

-- Position
-- Create list of tuples containing lemma and word position
-- "Position" depends on type-of-check, to re-use in "word pairs"

createPos :: String -> String -> [(String, Int)]
createPos s t = case t of
    "word"      -> createWordPos s
    "line"      -> createLinePos s
    "percntage" -> createWordPos s
    _           -> createWordPos s

createWordPos :: String -> [(String, Int)]
createWordPos s  = zip (words s) [1..]

createLinePos :: String -> [(String, Int)]
createLinePos s = createWordLinePos s

--
-- Line coordinate
-- Create list of tuples containing lemma and line position
createWordLinePos :: String -> [(String,Int)]
createWordLinePos xs = setWordLines $ zip (getWordLines xs) [1..]

-- Makes lists of words by line, fed to the functions below
getWordLines :: String -> [[String]]
getWordLines xs = fmap words (lines xs)

setWordLine :: ([String],Int) -> [(String, Int)]
setWordLine (([],_)) = []
setWordLine ((x:xs,i)) = (x,i) : (setWordLine (xs,i))

setWordLines :: [([String],Int)] -> [(String,Int)]
setWordLines [] = []
setWordLines (x:xs) = (setWordLine x) ++ (setWordLines xs)

--Column coordinate
--1) lines on file and zip infinite lists for each char. [[(Char,Int)]]
--2) compare first char of words from each inner [[String]] 
--   and add Int from [[(Char,Int)]] when Char == first Char of string

createWordColPos :: String -> [(String, Int)]
createWordColPos xs = setWordCols lin $ numWordCols $ lin
  where lin = lines xs

-- number columns in each line of file
numWordCols :: [String] -> [[(Char,Int)]]
numWordCols [[]] = [[]]
numWordCols (x:xs) = [zip x [1..]] ++ numWordCols xs

-- call filter on lines
setWordCols :: [String] -> [[(Char,Int)]] -> [(String,Int)]
setWordCols (x:xs) (y:ys) = filtWordCols (words x) y ++ setWordCols xs ys

filtWordCols :: [String] -> [(Char,Int)] -> [(String,Int)]
filtWordCols [] _ = []
filtWordCols _ [] = []
filtWordCols w@(x:xs) c@(y:ys) = if (fst y == ' ') || (fst y /= head x)
                         then filtWordCols w ys
                         else [(x,snd y)] ++ (filtWordCols xs $ drop (length x) c )

--
-- Master functoion to create a list of words from a file.    
--
zipWords :: String -> String -> Words
zipWords s t = zipWith4 (\w x y z -> Word w x y z) (words s) (wordpos) (linepos) (colpos)
  where
    wordpos = case t of 
        "word" -> snd . unzip $ createPos s "word"
        "line" -> snd . unzip $ createPos s "line"
        -- "percentage" -> snd . unzip $ createPos s "percentage" 
    linepos = snd . unzip $ createWordLinePos s
    colpos = snd . unzip $ createWordColPos s

--
-- Functions to operate on lists of words.
--
-- Check a word against the minimum word length (wordlength Arguments in WordcheckArgs)
isCheckWordLong :: Word -> Int -> Bool
isCheckWordLong (Word w _ _ _) x = length w > x

-- Filter Words based on minimum word length
checkWordList :: Words -> Int -> Words
checkWordList [] _ = []
checkWordList (x:xs) i = if isCheckWordLong x i
                     then x : checkWordList xs i
                     else checkWordList xs i

-- Equality function checking for string match in different coordinate positions
checkWordEquality :: Word -> Word -> Bool
checkWordEquality (Word a _ b c) (Word x _ y z) = (b /= y && c /= z) && a==x

-- Function to determine distance between two words
-- Uses Position of Word so it is type-of-search independent
checkWordDistance :: Word -> Word -> Int
checkWordDistance (Word _ x _ _) (Word _ y _ _) = x - y

-- Filter all but matching pairs of words
filterMatchingWords :: Words -> Words
filterMatchingWords [] = []
filterMatchingWords xs = sortBy compare $ intersectBy checkWordEquality xs xs
