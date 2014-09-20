module WordcheckWordpairs where
import WordcheckWords

--
-- This module contains types and functions for working with pairs of matching
-- words and the distances between them. The key type is a Wordpair, which
-- contains two Word elements and the difference between their Positions.
--

data Wordpair = Wordpair 
                {wone  :: Word
                ,wtwo  :: Word
                ,pdiff :: Int 
                } deriving (Show) 

type Wordpairs = [Wordpair]

makeWordpairs :: Word -> Word -> Wordpair
makeWordpairs wx@(Word _ x _ _) wy@(Word _ y _ _) = Wordpair wx wy (y-x)

-- Convert a list of matching (but separately-located) Words into a list of
-- Wordpair elements. This counts by two

sortWordsByString :: Words -> Wordpairs
sortWordsByString [] = []
sortWordsByString [_] = []
sortWordsByString [x,xs] = if x `checkWordEquality` xs    
                               then [makeWordpairs x xs]
                               else []
sortWordsByString ( x:y:xs ) = if x `checkWordEquality` y
                               then makeWordpairs x y : (sortWordsByString (y:xs))
                               else sortWordsByString $ (y:xs)

-- When run without the --all flag, filter by distance between matches

filterWordpairsByDistance :: Wordpairs -> Int -> Wordpairs
filterWordpairsByDistance [] _ = []
filterWordpairsByDistance (x:xs) 0 = if pdiff x <= 50
                               then x : filterWordpairsByDistance xs 0
                               else filterWordpairsByDistance xs 0

filterWordpairsByDistance (x:xs) i = if pdiff x <= i
                               then x : filterWordpairsByDistance xs i
                               else filterWordpairsByDistance xs i

-- Functions to extract data from wordpairs

getWordPairString :: Wordpair -> String
getWordPairString wp = if wordone == wordtwo
                        then show wordone
                        else
                        "Error with" ++ (show wp)
  where 
    wordone = lemma $ wone wp
    wordtwo = lemma $ wtwo wp

getWordpairPositions :: Wordpair -> (Int,Int)
getWordpairPositions wp = ((position $ wone wp),(position $ wtwo wp))

getWordpairLines :: Wordpair -> (Int,Int)
getWordpairLines wp = ((line $ wone wp),(line $ wtwo wp))

-- return ((Line,Col)(Line,Col))
getWordpairCoords :: Wordpair -> ((Int,Int),(Int,Int))
getWordpairCoords wp = (((line firstword),(column firstword)),((line secondword),(column secondword)))
    where
      firstword = wone wp
      secondword = wtwo wp
