module WordcheckWordpairs where

import WordcheckWords

--
-- This module contains types and functions for working with pairs of matching
-- words and the distances between them. The key type is a Wordpair, which
-- contains two Word elements and the difference between their Positions.
--

data Wordpair a = Wordpair 
                {wone  :: Word a
                ,wtwo  :: Word a
                ,pdiff :: a 
                } 

type Wordpairs a = [Wordpair a]

makeWordpairs :: (Num a, NumOps a) => Word a -> Word a -> Wordpair a
makeWordpairs wx@(Word _ x _ _) wy@(Word _ y _ _) = Wordpair wx wy (y-x)

-- Convert a list of matching (but separately-located) Words into a list of
-- Wordpair elements. This counts by two

sortWordsByString :: (Num a, NumOps a) =>  Words a -> Wordpairs a
sortWordsByString [] = []
sortWordsByString [_] = []
sortWordsByString [x,xs] =  [makeWordpairs x xs | x `checkWordEquality` xs] 
  -- if x `checkWordEquality` xs    
  --                              then [makeWordpairs x xs]
  --                              else []
sortWordsByString ( x:y:xs ) = if x `checkWordEquality` y
                               then makeWordpairs x y : sortWordsByString (y:xs)
                               else sortWordsByString (y:xs)

-- When run without the --all flag, filter by distance between matches

filterWordpairsByDistance :: (Num a, Eq a, Ord a, NumOps a) => Wordpairs a -> a -> Wordpairs a
filterWordpairsByDistance [] _ = []
-- filterWordpairsByDistance (x:xs) _ = if pdiff x <= 50
--                                then x : filterWordpairsByDistance xs 0
--                                else filterWordpairsByDistance xs 0

filterWordpairsByDistance (x:xs) i = if pdiff x <= i
                               then x : filterWordpairsByDistance xs i
                               else filterWordpairsByDistance xs i

-- Functions to extract data from wordpairs

getWordPairString :: Wordpair a -> String
getWordPairString wp = if wordone == wordtwo
                        then wordone
                        else
                        "Error with word pair"
  where 
    wordone = lemma $ wone wp
    wordtwo = lemma $ wtwo wp

getWordpairPositions :: (NumOps a) => Wordpair a -> (a,a)
getWordpairPositions wp = (position $ wone wp,position $ wtwo wp)

getWordpairLines :: (NumOps a) => Wordpair a -> (Int,Int)
getWordpairLines wp = (line $ wone wp,line $ wtwo wp)

-- return ((Line,Col)(Line,Col))
getWordpairCoords :: (NumOps a) => Wordpair a -> ((Int,Int),(Int,Int))
getWordpairCoords wp = ((line firstword,column firstword),(line secondword,column secondword))
    where
      firstword = wone wp
      secondword = wtwo wp
