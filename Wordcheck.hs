import System.Console.CmdArgs

--Wordcheck modules
import WordcheckArgs
import WordcheckWords

main :: IO ()
main = do
    -- Execute command line arguments
    cargs <- cmdArgs cliargs 
    putStrLn $ "\n" ++ "Running a " ++ type_ cargs ++ "-based check on " ++ file cargs
    putStrLn $ "With a minimum distance of " ++ show (distance cargs) ++ " between words of length " 
                ++ show (wordlength cargs) ++ "\n"
    -- print cargs
    dat <- accessData . checkFileStdin $ file cargs
    let dist = checkDistanceOrAll cargs
    let wordlen = wordlength cargs
    case type_ cargs  of
        [] -> do 
            let checkedwords = runWordCheck dat wordlen dist
            putStrLn "\nNo type selected; running word-based check.]\n"
            mapM_ print (processWordData checkedwords)
            putStrLn ""
            putStrLn $ "Found " ++ show (length checkedwords)  ++ " pairs of words "
                        ++ show (wordlength cargs) ++ " or more characters in length out of "
                        ++ show (length dat) ++ " words total."
        "word" -> do
            let checkedwords = runWordCheck dat wordlen dist
            mapM_ print (processWordData checkedwords)
            putStrLn ""
            summaryData (length checkedwords) (wordlength cargs) (length dat) 
        "line" -> do
            let checkedwords = runLineCheck dat wordlen dist
            mapM_ print (processLineData checkedwords)
            putStrLn ""
            summaryData (length checkedwords) (wordlength cargs) (length dat) 
        "percentage" -> do 
            putStrLn "No percentage check implemented."
    

runWordCheck :: String -> Int -> Maybe Int -> Wordpairs
runWordCheck xs a b = case b of
    Nothing -> sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "word") $ a 
    Just i -> filterWordpairsByDistance (sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "word") $ a) i

processWordData :: Wordpairs -> [String]
processWordData [] = []
processWordData (x:xs) = ((getWordPairString x) ++ " at positions "
                         ++ show (getWordpairPositions x)
                         ++ " at coordinates " ++ show (getWordpairCoords x)
                         ++ " with an intervening distance of "
                         ++ show (pdiff x) ++ " words") : processWordData xs

runLineCheck :: String -> Int -> Maybe Int -> Wordpairs
runLineCheck xs a b = case b of
    Nothing -> sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "line") $ a 
    Just i -> filterWordpairsByDistance (sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "line") $ a) i

processLineData :: Wordpairs -> [String]
processLineData [] = []
processLineData (x:xs) = ((getWordPairString x) 
                         ++ " at coordinates " ++ show (getWordpairCoords x)
                         ++ " with an intervening distance of "
                         ++ show (pdiff x) ++ " lines") : processLineData xs

summaryData :: Int -> Int -> Int -> IO()
summaryData x y z = do
            putStrLn ""
            putStrLn $ "Found " ++ show x  ++ " pairs of words "
                        ++ show y ++ " or more characters in length out of "
                        ++ show z ++ " words total."

