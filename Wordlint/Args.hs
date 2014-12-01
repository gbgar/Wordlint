{-# LANGUAGE DeriveDataTypeable #-}

module Wordlint.Args where
import System.Console.CmdArgs 
import Control.Monad
import Wordlint.Words
import Wordlint.Wordpairs

-- CLI Arguments
-- Data structure and init function for CmdArgs
data Arguments  = Arguments 
        {file :: String
        -- linting options
        ,wordlength :: Int
        ,type_ :: String
        ,distance :: String
        -- filters
        ,nocaps :: Bool
        ,nopunct :: Bool -- ,ignore-punctuation :: Bool goddammit
        ,blacklist :: String
        -- output options
        ,all_ :: Bool
        ,human :: Bool
        ,sort_ :: String
        }
        deriving (Data, Typeable, Show, Read)

cliargs :: Arguments
cliargs = Arguments
        {file = ""          &= help "If not present, read from stdin" &= typFile
        -- linting options
        ,wordlength = 5      &= help   "Minimum length of matched words" &= typ "Int"
        ,type_      = "word" &= help   "Type of distance (accepts \"word\", \"line\", or \"percentage\")" 
                             &= typ    "word|line|percentage"
        ,distance   = "250"  &= help   "Maximum distance between matches. Accepts integer for word and line; float (i.e. 0.75) for percentage)"

        -- filters
        ,nocaps    = False  &= help "Ignore capitalization when finding matches."
        ,nopunct   = False  &=  help "Ignore punctuation when finding matches."
        ,blacklist = ""     &=  help "File with newline-separated list of words to filter from output." 
                            &= typFile

        -- output options
        ,all_   = False      &= help "Show all matched results regardless of intervening distance"
        ,human  = False      &= help "Print resutlts in human-readable form."
        ,sort_  = "position" &= help "Sort alphabetically, by position, or by intervening distance"
                             &= typ "word|position|distance|error"
        } 
        &= help "wordlint [OPTION]...[-f FILE]..."
        &= summary "Wordlint v0.1.0.2 Gardner 2014 WTFPL"
        &= details ["Wordlint finds pairs of repeated words within a given"
                   ,"numerical range of words, lines or percentage of the input."
                   ,"This should be useful to curb redundancy in prose."]
--------------------------------------------------------------------------------
--
-- Input file/stdin functions
--
--------------------------------------------------------------------------------
checkFileStdin :: String -> Maybe String
checkFileStdin s | null s = Nothing 
                 | otherwise = Just s


accessInputFileData :: Maybe String -> IO String
accessInputFileData f =
    case f of
         Nothing -> getContents
         Just fp -> readFile fp 

accessBlacklistFileData :: Maybe String -> IO String
accessBlacklistFileData f =
    case f of
        Nothing -> return ""
        Just fp -> readFile fp 

setBlacklistData :: String -> Maybe [String]
setBlacklistData a | null a = Nothing
                   | otherwise = Just $ lines a


--------------------------------------------------------------------------------
--
-- Filter functions
--
--------------------------------------------------------------------------------

runFilterFlags :: (NumOps a) => Words a -> Arguments -> Maybe [String] -> Words a
runFilterFlags w arg blist = runCapsFilter arg $ runBlacklistFilter blist $ runPunctFilter arg $ runBlacklistFilter blist w

runPunctFilter :: (NumOps a) => Arguments -> Words a -> Words a
runPunctFilter arg wordlist = if nopunct arg
                           then filterWordPunctuation wordlist
                           else wordlist

runCapsFilter :: (NumOps a) => Arguments -> Words a -> Words a
runCapsFilter arg wordlist = if nocaps arg
                           then filterWordCapitalization wordlist
                           else wordlist

runBlacklistFilter :: (NumOps a) => Maybe [String] -> Words a -> Words a
runBlacklistFilter blist wordlist = case blist of
                                        Nothing -> wordlist
                                        Just x -> filterWordBlacklist wordlist x

--------------------------------------------------------------------------------
--
-- Output flag functions
--
--------------------------------------------------------------------------------

-- Handle --all flag OR read --distance 
checkDistanceOrAll :: (Read a, NumOps a) => Arguments -> Maybe a
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
                   | x == "error" = sortWordPairsByPosition y
                   | otherwise = y
                  
