import System.Console.CmdArgs
import WordcheckArgs
import WordcheckWords
import WordcheckWordpairs

main :: IO ()
main = do
    -- Execute command line arguments
    cargs <- cmdArgs cliargs
    dat <- accessData . checkFileStdin $ file cargs
    putStrLn $ "Running a " ++ type_ cargs
      ++ "-based check on " ++ file cargs
    putStrLn $ "With a minimum distance of "
      ++ show (distance cargs) ++ " between words of length " 
      ++ show (wordlength cargs) ++ "\n"
    let wordlen = wordlength cargs
    case type_ cargs  of
        [] -> do 
            let checkedwords = runWordCheck dat wordlen dist
            putStrLn "No type selected; running word-based check.\n"
            mapM_ putStrLn (processWordData checkedwords)
            putStrLn ""
            summaryData (length checkedwords) (wordlength cargs) (length dat) 
                where dist = checkDistanceOrAll cargs
        "word" -> do
            let checkedwords = runWordCheck dat wordlen dist
            mapM_ putStrLn (processWordData checkedwords)
            putStrLn ""
            summaryData (length checkedwords) (wordlength cargs) (length dat) 
                where dist = checkDistanceOrAll cargs 
        "line" -> do
            let checkedwords = runLineCheck dat wordlen dist
            mapM_ putStrLn (processLineData checkedwords)
            putStrLn ""
            summaryData (length checkedwords) (wordlength cargs) (length dat) 
                where dist = checkDistanceOrAll cargs
        "percentage" -> do 
            let checkedwords = runPercentageCheck dat wordlen dist
            mapM_ putStrLn (processPercentageData checkedwords)
            putStrLn ""
            summaryData (length checkedwords) (wordlength cargs) (length dat) 
                where dist = checkDistanceOrAll cargs

-- run*Check functions are run with a maybe to account for --all flag
-- process*Data functions return a human-readable format. 
-- In the future, a "machine-readable" flag will be added to the latter
-- in order to output for use in text editor plugins.
        
runWordCheck :: String -> Int -> Maybe Int -> Wordpairs Int 
runWordCheck xs y b = case b of
    Nothing -> instring y 
    Just i -> filterWordpairsByDistance (instring y) i
    where instring = sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "word") 

processWordData :: Wordpairs Int -> [String]
processWordData [] = ["No (more) matches found"]
processWordData (x:xs) = ("\'" ++ word ++ "\'"
                         ++ " at positions "
                         ++ position'
                         ++ " at coordinates "
                         ++ coordinates
                         ++ " with an intervening distance of "
                         ++ distance'
                         ++ " words") : processWordData xs
                         where word = getWordPairString x
                               position' = show (getWordpairPositions x)
                               coordinates = show (getWordpairCoords x)
                               distance' = show (pdiff x)


runLineCheck :: String -> Int -> Maybe Int -> Wordpairs Int
runLineCheck xs y b = case b of
    Nothing ->  instring y 
    Just i -> filterWordpairsByDistance (instring y) i
    where instring = sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "line") 

processLineData :: Wordpairs Int -> [String]
processLineData [] = ["No (more) matches found"]
processLineData (x:xs) = ("\'" ++ word ++ "\'"
                         ++ " at coordinates "
                         ++ coordinates
                         ++ " with an intervening distance of "
                         ++ distance'
                         ++ " lines") : processLineData xs
                         where word = getWordPairString x
                               coordinates = show (getWordpairCoords x)
                               distance' = show (pdiff x)

runPercentageCheck :: String -> Int -> Maybe Double -> Wordpairs Double
runPercentageCheck xs y b = case b of
    Nothing ->  instring y 
    Just i -> filterWordpairsByDistance (instring y) i
    where instring = sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "percentage") 

processPercentageData :: Wordpairs Double -> [String]
processPercentageData [] = ["No (more) matches found"]
processPercentageData (x:xs) = ("\'" ++ word ++ "\'"
                         ++ " at coordinates "
                         ++ coordinates
                         ++ " with an intervening distance of %"
                         ++ distance') : processPercentageData xs
                         where word = getWordPairString x
                               coordinates = show (getWordpairCoords x)
                               distance' = take 5 (show (pdiff x))


summaryData :: Int -> Int -> Int -> IO()
summaryData x y z = do
            putStrLn ""
            putStrLn $ "Found " ++ show x  ++ " pairs of words "
              ++ show y ++ " or more characters in length out of "
              ++ show z ++ " words total."
