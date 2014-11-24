import Data.List
import System.Console.CmdArgs
import Text.PrettyPrint.Boxes 
import Wordlint.Args
import Wordlint.Words
import Wordlint.Wordpairs

main :: IO ()
main = do
    -- Execute command line arguments, retrieve flags
    cargs <- cmdArgs cliargs
    let sortflag = sort_ cargs
    let wordlen = wordlength cargs
    -- If human-readable flag is present, print header
    checkIfHumanHeader cargs
    -- Acquire String data from file or stdin
    dat <- accessData . checkFileStdin $ file cargs
    -- 
    -- Choose checker and printer according to -t, -h flags
    -- 
    case type_ cargs  of
        [] -> do 
            let checkedwords' = runWordCheck dat wordlen dist
            let checkedwords =  checkSortFlag sortflag checkedwords'
            if human cargs 
            then do putStrLn "No type chosen; running word-based check"
                    mapM_ putStrLn (processHumanWordData checkedwords)
                    putStrLn ""
                    summaryData (length checkedwords) (wordlength cargs) (length dat)
            else 
                if sortflag == "vim" 
                then putStrLn $ processDataVim checkedwords
                else processMachineWordData checkedwords
                             where dist = checkDistanceOrAll cargs
        "word" -> do
            let checkedwords' = runWordCheck dat wordlen dist
            let checkedwords =  checkSortFlag sortflag checkedwords'

            if human cargs
            then do mapM_ putStrLn (processHumanWordData checkedwords) 
                    putStrLn ""
                    summaryData (length checkedwords) (wordlength cargs) (length dat) 
            else 
                if sortflag == "vim"
                then putStrLn $ processDataVim checkedwords
                else processMachineWordData checkedwords
                            where dist = checkDistanceOrAll cargs 
        "line" -> do
            let checkedwords' = runLineCheck dat wordlen dist
            let checkedwords =  checkSortFlag sortflag checkedwords'
            if human cargs
            then do mapM_ putStrLn (processHumanLineData checkedwords)
                    putStrLn ""
                    summaryData (length checkedwords) (wordlength cargs) (length dat)
            else 
                if sortflag == "vim"
                then putStrLn $ processDataVim checkedwords
                else processMachineLineData checkedwords
                            where dist = checkDistanceOrAll cargs
        "percentage" -> do 
            let checkedwords' = runPercentageCheck dat wordlen dist
            let checkedwords =  checkSortFlag sortflag checkedwords'
            if human cargs
            then do mapM_ putStrLn (processHumanPercentageData checkedwords)
                    putStrLn ""
                    summaryData (length checkedwords) (wordlength cargs) (length dat)
            else 
                if sortflag == "vim"
                then putStrLn $ processDataVim checkedwords
                else processMachinePercentageData checkedwords
                            where dist = checkDistanceOrAll cargs

        _ -> do 
            let checkedwords' = runWordCheck dat wordlen dist
            let checkedwords =  checkSortFlag sortflag checkedwords'
            if human cargs 
            then do putStrLn "No type chosen; running word-based check"
                    mapM_ putStrLn (processHumanWordData checkedwords)
                    putStrLn ""
                    summaryData (length checkedwords) (wordlength cargs) (length dat)
            else 
                if sortflag == "vim" 
                then putStrLn $ processDataVim checkedwords
                else processMachineWordData checkedwords
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

processHumanWordData :: Wordpairs Int -> [String]
processHumanWordData [] = ["No (more) matches found"]
processHumanWordData (x:xs) = ("\'" ++ word ++ "\'"
                                    ++ " at positions "
                                    ++ position'
                                    ++ " at coordinates "
                                    ++ coordinates
                                    ++ " with an intervening distance of "
                                    ++ distance'
                                    ++ " words") : processHumanWordData xs
                         where word = getWordPairString x
                               position' = show (getWordpairPositions x)
                               coordinates = show (getWordpairCoords x)
                               distance' = show (pdiff x)

-- machine-based outputs drawn from
-- http://www.tedreed.info/programming/2012/06/02/how-to-use-textprettyprintboxes/
processMachineWordData :: Wordpairs Int -> IO ()
processMachineWordData x = printBox $ hsep 2 left (map (vcat left . map text) (transpose $ processMachineWordData' x))

processDataVim :: (NumOps a) => Wordpairs a -> String
processDataVim [] = ""
processDataVim (x:xs) = ("lnum=" ++ "\'" ++ linum1 ++ "\', "  ++ "col=" ++ "\'" ++ colnum1 ++ "\', " ++ "text=" ++ "\'" ++ word ++ "\'\n") ++
                        ("lnum=" ++ "\'" ++ linum2 ++ "\', "  ++ "col=" ++ "\'" ++ colnum2 ++ "\', " ++ "text=" ++ "\'" ++ word ++ "\'\n") ++
                        processDataVim xs
                         where word = getWordPairString x
                               coordinates' = getWordpairCoords x
                               coordinates1 ((r1,_),(_,_)) = show r1
                               coordinates1' ((_,_),(r1,_)) = show r1
                               linum1 = coordinates1 coordinates'
                               linum2 = coordinates1' coordinates'
                               coordinates2 ((_,s1),(_,_)) = show s1 
                               coordinates2' ((_,_),(_,s1)) = show s1 
                               colnum1 = coordinates2 coordinates'
                               colnum2 = coordinates2' coordinates'
                               

processMachineWordData' :: Wordpairs Int -> [[String]]
processMachineWordData' [] = []
processMachineWordData' (x:xs) = words (coordinates1 coordinates'
                                 ++ " "
                                 ++ coordinates2 coordinates'
                                 ++ " "
                                 ++ word 
                                 ++ " "
                                 ++ distance') : processMachineWordData' xs
                         where word = getWordPairString x
                               coordinates' = getWordpairCoords x
                               coordinates1 ((r1,s1),(_,_)) = show r1 ++ "," ++ show s1
                               coordinates2 ((_,_),(r2,s2)) = show r2 ++ "," ++ show s2
                               distance' = show (pdiff x)

runLineCheck :: String -> Int -> Maybe Int -> Wordpairs Int
runLineCheck xs y b = case b of
    Nothing ->  instring y 
    Just i -> filterWordpairsByDistance (instring y) i
  where instring = sortWordPairsByPosition . sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "line") 


processHumanLineData :: Wordpairs Int -> [String]
processHumanLineData [] = ["No (more) matches found"]
processHumanLineData (x:xs) = ("\'" ++ word ++ "\'"
                         ++ " at coordinates "
                         ++ coordinates
                         ++ " with an intervening distance of "
                         ++ distance'
                         ++ " lines") : processHumanLineData xs
                         where word = getWordPairString x
                               coordinates = show (getWordpairCoords x)
                               distance' = show (pdiff x)
                               
processMachineLineData :: Wordpairs Int -> IO ()
processMachineLineData x = printBox $ hsep 2 left (map (vcat left . map text) (transpose $ processMachineLineData' x))

processMachineLineData' :: Wordpairs Int -> [[String]]
processMachineLineData' [] = []
processMachineLineData' (x:xs) = words (coordinates1 coordinates' 
                                 ++ " "
                                 ++ coordinates2 coordinates'
                                 ++ " "
                                 ++ word 
                                 ++ " "
                                 ++ distance')
                                 : processMachineLineData' xs
                         where word = getWordPairString x
                               coordinates' = getWordpairCoords x
                               coordinates1 ((r1,s1),(_,_)) = show r1 ++ "," ++ show s1
                               coordinates2 ((_,_),(r2,s2)) = show r2 ++ "," ++ show s2
                               distance' = show (pdiff x)

runPercentageCheck :: String -> Int -> Maybe Double -> Wordpairs Double
runPercentageCheck xs y b = case b of
    Nothing ->  instring y 
    Just i -> filterWordpairsByDistance (instring y) i
    where instring = sortWordsByString . filterMatchingWords . checkWordList (zipWords xs "percentage") 

processHumanPercentageData :: Wordpairs Double -> [String]
processHumanPercentageData [] = ["No (more) matches found"]
processHumanPercentageData (x:xs) = ("\'" ++ word ++ "\'"
                         ++ " at coordinates "
                         ++ coordinates
                         ++ " with an intervening distance of %"
                         ++ distance') : processHumanPercentageData xs
                         where word = getWordPairString x
                               coordinates = show (getWordpairCoords x)
                               distance' = take 7 (show (pdiff x))

processMachinePercentageData :: Wordpairs Double -> IO ()
processMachinePercentageData x = printBox $ hsep 2 left (map (vcat left . map text) 
                                    (transpose $ processMachinePercentageData' x))

processMachinePercentageData' :: Wordpairs Double -> [[String]]
processMachinePercentageData' [] = []
processMachinePercentageData' (x:xs) = words (coordinates1 coordinates'
                                    ++ " "
                                    ++ coordinates2 coordinates'
                                    ++ " "
                                    ++ word
                                    ++ " "
                                    ++ distance') : processMachinePercentageData' xs
                                      where word = getWordPairString x
                                            coordinates' = getWordpairCoords x
                                            coordinates1 ((r1,s1),(_,_)) = show r1 ++ "," ++ show s1
                                            coordinates2 ((_,_),(r2,s2)) = show r2 ++ "," ++ show s2
                                            distance' = take 7 $ show (pdiff x)

-- Function that provides summary totals when -h flag is passed
summaryData :: Int -> Int -> Int -> IO()
summaryData x y z = do
            putStrLn ""
            putStrLn $ "Found " ++ show x  ++ " pairs of words "
              ++ show y ++ " or more characters in length out of "
              ++ show z ++ " words total."
