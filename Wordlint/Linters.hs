module Wordlint.Linters where

import Control.Monad
import Data.List
import Data.Function 
import Wordlint.Args
import Wordlint.Words
import Wordlint.Wordpairs

-- Linter data type which holds external input and parameters
-- necessary to easily 
data Linter = Linter {
  inputdata        :: String
  ,wordlength      :: Int
  ,allornot        :: Bool
  ,maybeblacklist  :: Maybe [String]
  ,word'           :: Int
  ,line'           :: Int
  ,percent'        :: Double
  ,args            :: Arguments
  ,result          :: Wordpairs Double
}

-- Get and run linters
getLinter :: Arguments -> IO Linter
getLinter cargs = do
  -- get filename
    dat <- accessInputFileData . checkFileStdin $ file cargs
    blist <- liftM setBlacklistData $ accessBlacklistFileData. checkFileStdin $ blacklist cargs
    let mlen = matchlength cargs
    let isall = all_ cargs 
    let w' = words_ cargs
    let l = lines_ cargs
    let p = percent_ cargs
    let w = if w' == 0 && l == 0 && p == 0
               then 250
               else w'
    return $ Linter dat mlen isall blist w l p cargs []

runAllLinters :: Linter -> Wordpairs Double
runAllLinters linter = intersectWordpairs y
  where largs  = args linter
        words' = distorall linter (getWordpairListWords linter) (words_ largs)
        cwords = commensurateWordpairs words'
        lines' = distorall linter (getWordpairListLines linter) (lines_ largs)
        clines = commensurateWordpairs lines'
        perc   = distorall linter (getWordpairListPercent linter) (percent_ largs)
        y      = sortBy (flip (compare `on` length)) [cwords,clines,perc]

intersectWordpairs :: [Wordpairs Double] -> Wordpairs Double
intersectWordpairs [] = []
intersectWordpairs [a,[],[]] = a
intersectWordpairs [a,b,[]] = a `intersect` b
intersectWordpairs [a,b,c] = a `intersect` intersect b c
intersectWordpairs _ = []


getWordpairList :: (Num a, NumOps a) => Linter -> String -> Wordpairs a
getWordpairList linter type' = instring
  where instring = sortWordsByString . filterMatchingWords $ dwords
        dwords   = checkWordList fwords wordlen
        fwords   = runFilterFlags cwords arg blist
        cwords   = zipWords in' type'
        wordlen  = wordlength linter
        in' = inputdata linter
        arg     = args linter
        blist    = maybeblacklist linter

distorall :: (Eq a, Ord a, Num a, NumOps a) => Linter -> Wordpairs a -> a -> Wordpairs a
distorall linter wordpairs num = 
  if allornot linter
  then wordpairs
  else filterWordpairsByDistance wordpairs num

getWordpairListWords :: Linter -> Wordpairs Int
getWordpairListWords l = if word' l /= 0 then getWordpairList l "word" else []

getWordpairListLines :: Linter -> Wordpairs Int
getWordpairListLines l = if line' l /= 0 then getWordpairList l "line" else []

getWordpairListPercent :: Linter -> Wordpairs Double
getWordpairListPercent l = if percent' l /= 0 then getWordpairList l "percent" else []

commensurateWordpairs :: Wordpairs Int -> Wordpairs Double
commensurateWordpairs = map commensurateWordpair

commensurateWordpair :: Wordpair Int -> Wordpair Double
commensurateWordpair x = Wordpair a b c
                         where a = commensurateWords $ wone x
                               b = commensurateWords $ wtwo x
                               c = fromIntegral $ pdiff x

commensurateWords :: Word Int -> Word Double
commensurateWords x = Word (lemma x) pos (line x) (column x)
  where pos = fromIntegral $ position x
