{-# LANGUAGE DeriveDataTypeable #-}
module WordcheckArgs where
import System.Console.CmdArgs 

--CLI Arguments
data Arguments = Arguments 
        {wordlength :: Int
        ,type_ :: String
        ,distance :: Int
        ,all_ :: Bool
        ,file :: String
        }
        deriving (Data, Typeable, Show, Read)

cliargs :: Arguments
cliargs = Arguments
        {wordlength = 5 &= help "Minimum length of matched words"
        ,type_ = "word" &= help "Type of distance (word, line, percentage)"
        ,distance = 20 &= help "Maximum distance between matches (whole numbers only)"
        ,all_ = False &= help "Show all matched results regardless of intervening distance"
        ,file = "" &= help "If not present, read from stdin" &= typFile
        } &=
        help "wordcheck [-w word length] [-t word|line|percentage] [-d distance to match] [-f file]" &=
        summary "Wordcheck v0.0.1 Gardner 2014 WTFPL" &=
        details ["Wordcheck finds pairs of (preferably longer) words within a file"
                ,"given a numerical range in words, lines or percentage of the file."
                ,"This should be useful to curb redundancy in prose."]

--Functions to handle file/stdin
checkFileStdin :: String -> Maybe String
checkFileStdin s = if null s then Nothing else Just s

accessData :: Maybe String -> IO String
accessData f =
    case f of
         Nothing -> do
             putStrLn $ "Reading from stdin" ++ "\n"
             getContents
         Just fp -> do
             putStrLn $ "Reading from file" ++ "\n"
             readFile fp 

--Handles checking of --all flag
checkDistanceOrAll :: Arguments -> Maybe Int
checkDistanceOrAll a = if all_ a then Nothing else Just (distance a)
