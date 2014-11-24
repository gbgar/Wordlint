{-# LANGUAGE DeriveDataTypeable #-}

module Wordlint.Args where
import System.Console.CmdArgs 
import Control.Monad
import Wordlint.Words
import Wordlint.Wordpairs


-- CLI Arguments
-- Data structure and init function for CmdArgs
data Arguments  = Arguments 
        {wordlength :: Int
        ,type_ :: String
        ,distance :: String
        ,all_ :: Bool
        ,human :: Bool
        ,sort_ :: String
        ,file :: String
        }
        deriving (Data, Typeable, Show, Read)

cliargs :: Arguments
cliargs = Arguments
        {wordlength = 5 &= help "Minimum length of matched words" &= typ "Int"
        ,type_ = "word" &= help "Type of distance (accepts \"word\", \"line\", or \"percentage\")" 
                        &= typ "word|line|percentage"
        ,distance = "250" &= help "Maximum distance between matches (accepts Int for word and line; Float for percentage)"
                        &= typ "Integer|Float"
        ,all_ = False &= help "Show all matched results regardless of intervening distance"
        ,human = False &= help "Print resutlts in human-readable form."
        ,sort_ = "position" &= help "Sort alphabetically, by position, or by intervening distance"
                        &= typ "word|position|distance"
        ,file = "" &= help "If not present, read from stdin" &= typFile
        } 
        &= help "wordcheck [OPTION]...[-f FILE]..."
        &= summary "Wordlint v0.0.1 Gardner 2014 WTFPL"
        &= details ["Wordlint finds pairs of repeated words within a given"
                   ,"numerical range of words, lines or percentage of the input."
                   ,"This should be useful to curb redundancy in prose."]

-- Functions to handle file/stdin
checkFileStdin :: String -> Maybe String
checkFileStdin s = if null s then Nothing else Just s

accessData :: Maybe String -> IO String
accessData f =
    case f of
         Nothing -> getContents
         Just fp -> readFile fp 

-- Handle --all flag
checkDistanceOrAll :: (Read a) => Arguments -> Maybe a
checkDistanceOrAll c | all_ c = Nothing
                     | otherwise = Just (read $ distance c) 


-- Header to print if --human flag is present
checkIfHumanHeader :: Arguments -> IO ()
checkIfHumanHeader cargs = when (human cargs)
                     $ putStrLn ("Running a "
                                 ++ type_ cargs
                                 ++ "-based check on "
                                 ++ file cargs
                                 ++ "\nWith a minimum distance of "
                                 ++ show (distance cargs)
                                 ++ " between words of length "
                                 ++ show (wordlength cargs) ++ "\n")

-- Handle --sort flag
checkSortFlag :: (Num a, Ord a, NumOps a)  => String -> Wordpairs a -> Wordpairs a
checkSortFlag  x y | x == "position" = sortWordPairsByPosition y 
                   | x == "distance" = sortWordPairsByDistance y
                   | x == "word" = y
                   | x == "vim" = sortWordPairsByPosition y
                   | otherwise = y
                  
