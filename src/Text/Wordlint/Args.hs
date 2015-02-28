{-# LANGUAGE DeriveDataTypeable #-}

module Text.Wordlint.Args where

import System.Console.CmdArgs 
import Control.Monad
import Text.Wordlint.Words
import Text.Wordlint.Wordpairs

-- CLI Arguments
-- Data structure and init function for CmdArgs
data Arguments  = Arguments 
        {file :: String
        -- linting options
        ,matchlength :: Int
        ,words_ :: Int
        ,lines_ :: Int
        ,percent_ :: Double
        -- filters
        ,nocaps :: Bool
        ,nopunct :: Bool -- ,ignore-punctuation :: Bool goddammit
        ,blacklist :: String
        ,whitelist :: String
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
        ,matchlength = 5      &= help   "Minimum length of matched words" &= typ "Int"
        ,words_   = 0    &= help   "Maximum distance between matches - number of words. Default 250." &= typ "Int"
        ,lines_   = 0    &= help   "Maximum distance between matches - number of lines" &= typ "Int"
        ,percent_ = 0    &= help   "Maximum distance between matches - percentage of words." &= typ "Double"

        -- filters
        ,nocaps    = False  &= help "Ignore capitalization when finding matches."
        ,nopunct   = False  &=  help "Ignore punctuation when finding matches."
        ,blacklist = ""     &=  help "File with newline-separated list of words to filter from output." 
                            &= typFile
        ,whitelist = ""     &=  help "File with newline-separated list of words to allow in output." 
                            &= typFile

        -- output options
        ,all_   = False      &= help "Show all matched results regardless of intervening distance"
        ,human  = False      &= help "Print resutlts in human-readable form."
        ,sort_  = "position" &= help "Sort alphabetically, by position, or by intervening distance"
                             &= typ "word|position|distance|error"
        } 
        &= help "wordlint [OPTION]...[-f FILE]..."
        &= summary "Wordlint v0.2.1.0 Gardner 2014 WTFPL"
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
   Nothing ->  getContents
   Just fp ->  readFile fp 

accessListFileData :: Maybe String -> IO String
accessListFileData f =
    case f of
        Nothing -> return ""
        Just fp -> readFile fp 

setListData :: String -> Maybe [String]
setListData a | null a = Nothing
                   | otherwise = Just $ lines a
                                       

--------------------------------------------------------------------------------
--
-- Filter functions
--
--------------------------------------------------------------------------------

runFilterFlags :: (NumOps a) => Words a -> Arguments -> Maybe [String] -> Maybe [String] -> Words a
runFilterFlags w arg blist wlist = runWhitelistFilter wlist $runCapsFilter arg $ runBlacklistFilter blist $
                             runPunctFilter arg $ runBlacklistFilter blist w

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

runWhitelistFilter :: (NumOps a) => Maybe [String] -> Words a -> Words a
runWhitelistFilter wlist wordlist = case wlist of
                                        Nothing -> wordlist
                                        Just x -> filterWordWhitelist wordlist x
--------------------------------------------------------------------------------
--
-- Output flag functions
--
--------------------------------------------------------------------------------

-- Handle --sort flag
checkSortFlag :: (Num a, Ord a, NumOps a)  => String -> Wordpairs a -> Wordpairs a
checkSortFlag  x y | x == "position" = sortWordPairsByPosition y 
                   | x == "distance" = sortWordPairsByDistance y
                   | x == "word" = y
                   | x == "error" = sortWordPairsByPosition y
                   | otherwise = y
                  

-- Header to print if --human flag is present
checkIfHumanHeader :: Arguments -> IO ()
checkIfHumanHeader cargs = when (human cargs)
                     $ putStrLn ("Running "
                                 ++ getTypeOfCheck cargs
                                 ++ " check[s] on "
                                 ++ file cargs
                                 ++ "\nWith minimum distance[s] of "
                                 ++ getTypeOfCheck cargs
                                 ++ " between words of length "
                                 ++ show (matchlength cargs) ++ "\n")

getTypeOfCheck :: Arguments -> String
getTypeOfCheck c  = isWordsFlag c ++ " " ++ isLinesFlag c ++ " " ++ isPercentFlag c
  
  
getTypeNumbers :: Arguments -> String
getTypeNumbers c  = convertWordsFlag c ++ " " ++
                    convertLinesFlag c ++ " " ++ convertPercentFlag c


isWordsFlag :: Arguments -> String
isWordsFlag c | words_ c /= 0 ||
                words_ c == 0 && lines_ c == 0 && percent_ c == 0 = "word"
              | otherwise = ""
  
isLinesFlag :: Arguments -> String
isLinesFlag c | lines_ c /= 0 = "line"
              | otherwise = ""
  
isPercentFlag :: Arguments -> String
isPercentFlag c | percent_ c /= 0 = "percentage"
                | otherwise = ""

convertWordsFlag :: Arguments -> String
convertWordsFlag c | words_ c /= 0 = show $ words_ c
                   | otherwise = ""
       
convertLinesFlag :: Arguments -> String
convertLinesFlag c | lines_ c /= 0 = show $ lines_ c
                   | otherwise = ""
       
convertPercentFlag :: Arguments -> String
convertPercentFlag c | percent_ c /= 0 = show $ percent_ c
                     | otherwise = ""
