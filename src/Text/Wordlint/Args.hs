-- Copyright © 2014-2016 Blake Gardner github.com/gbgar
-- This file is part of Wordlint.

-- Wordlint is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Wordlint is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Wordlint.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable #-}

module Text.Wordlint.Args where

import Prelude hiding (Word)
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
        ,matchlength = 5      &= help   "Minimum length of matched words" &= typ "Int" &= groupname "Linting Options"
        ,words_   = 0    &= help   "Maximum distance between matches - number of words. Default 250." &= typ "Int" &= name "w" &= groupname "Linting Options"
        ,lines_   = 0    &= help   "Maximum distance between matches - number of lines" &= typ "Int" &= groupname "Linting Options"
        ,percent_ = 0    &= help   "Maximum distance between matches - percentage of words." &= typ "Double" &= groupname "Linting Options"

        -- filters
        ,nocaps    = False  &= help "Ignore capitalization when finding matches." &= groupname "Filter Options"
        ,nopunct   = False  &=  help "Ignore punctuation when finding matches." &= groupname "Filter Options"
        ,blacklist = ""     &=  help "File with newline-separated list of words to filter from output." 
                            &= typFile &= groupname "Filter Options"
        ,whitelist = ""     &=  help "File with newline-separated list of words to allow in output." 
                            &= typFile &= groupname "Filter Options"

        -- output options
        ,all_   = False      &= help "Show all matched results regardless of intervening distance" &= groupname "Output Options"
        ,human  = False      &= help "Print resutlts in human-readable form." &= groupname "Output Options"
        ,sort_  = "position" &= help "Sort alphabetically, by position, or by intervening distance" 
                             &= typ "word|position|distance|error" &= groupname "Output Options"
        } 
        &= help "wordlint [OPTION]...[-f FILE]..."
        &= summary "Wordlint v0.2.1.0 Gardner 2014-2016 WTFPL"
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
runFilterFlags w arg blist wlist = runWhitelistFilter wlist $ runCapsFilter arg $ runBlacklistFilter blist $ runPunctFilter arg $ runBlacklistFilter blist w

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
