module Wordlint.Linters where

import Control.Monad
import Data.List
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
}

-- CommensurateWord,CommensurateWordpair data types to strip Double/Int
-- from Word/Wordpair

getLinter :: Arguments -> IO Linter
getLinter cargs = do
  -- get filename
    dat <- accessInputFileData . checkFileStdin $ file cargs
    blist <- liftM setBlacklistData $ accessBlacklistFileData. checkFileStdin $ blacklist cargs
    let mlen = matchlength cargs
    let isall = all_ cargs 
    let w = words_ cargs
    let l = lines_ cargs
    let p = percent_ cargs
    return $ Linter dat mlen isall blist w l p cargs


runAllLinters :: Linter -> Wordpairs Double
runAllLinters l = z
  where w = commensurateWordpairs $ getWordpairListWords l
        l = commensurateWordpairs $ getWordpairListLines l
        p = getWordpairListPercent l
        r = sortBy (\x y -> compare (length x) (length y)) [w,l,p]
        z = intersect (head r) (intersect $ tail r)
        
          


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

getWordpairListWords :: Linter -> Wordpairs Int
getWordpairListWords l = if (word' l) /= 0 then getWordpairList l "word" else []

getWordpairListLines :: Linter -> Wordpairs Int
getWordpairListLines l = if (line' l) /= 0 then getWordpairList l "line" else []

getWordpairListPercent :: Linter -> Wordpairs Double
getWordpairListPercent l = if (percent' l) /= 0 then getWordpairList l "percent" else []

commensurateWordpairs :: Wordpairs Int -> Wordpairs Double
commensurateWordpairs [] = []
commensurateWordpairs (x:xs) = commensurateWordpair x : commensurateWordpairs xs

commensurateWordpair :: Wordpair Int -> Wordpair Double
commensurateWordpair x = Wordpair a b c
                         where a = commensurateWords $ wone x
                               b = commensurateWords $ wtwo x
                               c = fromIntegral $ pdiff x

commensurateWords :: Word Int -> Word Double
commensurateWords x = Word lemma x (fromIntegral . position x) line x column x
