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

module Text.Wordlint.Wordpairs where

import Prelude hiding (Word)
import Data.List 
import Text.Wordlint.Words

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

-- Wordpair equality based on coordinates so that
-- union of multiple checks can be found regardless
--
instance Eq (Wordpair a) where
         x == y = getWordpairCoords x == getWordpairCoords y
-----------------------------------------------------------------
--
-- Creating and filtering Wordpairs
--
-----------------------------------------------------------------
makeWordpairs :: (Num a, NumOps a) => Word a -> Word a -> Wordpair a
makeWordpairs wx@(Word _ x _ _) wy@(Word _ y _ _) = Wordpair wx wy (y-x)

-- Convert a list of matching (but separately-located) Words into a list of
-- Wordpair elements. This counts by two
sortWordsByString :: (Num a, NumOps a) =>  Words a -> Wordpairs a
sortWordsByString [] = []
sortWordsByString [_] = []
sortWordsByString [x,xs] =  [makeWordpairs x xs | x `checkWordEquality` xs] 
sortWordsByString ( x:y:xs ) = if x `checkWordEquality` y
                               then makeWordpairs x y : sortWordsByString (y:xs)
                               else sortWordsByString (y:xs)

filterWordpairsByDistance :: (Num a, Eq a, Ord a, NumOps a) => Wordpairs a -> a -> Wordpairs a
filterWordpairsByDistance [] _ = []

filterWordpairsByDistance (x:xs) i = if pdiff x <= i
                               then x : filterWordpairsByDistance xs i
                               else filterWordpairsByDistance xs i

-----------------------------------------------------------------
--
-- Sorting wordpairs
--
-----------------------------------------------------------------

sortWordPairsByPosition :: (Num a, Ord a, NumOps a) => Wordpairs a -> Wordpairs a
sortWordPairsByPosition [] = []
sortWordPairsByPosition [_] = []
sortWordPairsByPosition xs = sortBy positionsort xs
  where positionsort (Wordpair (Word _ x _ _) _ _ ) (Wordpair (Word _ y _ _) _ _) 
                | x < y = LT
                | x > y = GT
                | x == y = EQ
        positionsort Wordpair{} Wordpair{} = EQ

sortWordPairsByDistance :: (Num a, Ord a, NumOps a) => Wordpairs a -> Wordpairs a
sortWordPairsByDistance [] = []
sortWordPairsByDistance [_] = []
sortWordPairsByDistance xs = sortBy positionsort xs
  where positionsort (Wordpair _ _ x ) (Wordpair _ _ y ) 
                | x < y = LT
                | x > y = GT
                | x == y = EQ
        positionsort Wordpair{} Wordpair{} = EQ

-----------------------------------------------------------------
--
-- Functions to extract data from Wordpairs for easier printing
--
-----------------------------------------------------------------

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
getWordpairCoords :: Wordpair a -> ((Int,Int),(Int,Int))
getWordpairCoords wp = ((line firstword,column firstword),(line secondword,column secondword))
    where
      firstword = wone wp
      secondword = wtwo wp

showFirstWordpairCoords :: (Show a) => Wordpair a -> String
showFirstWordpairCoords x = lin ++ "," ++ col
  where lin = show $ line wor
        col = show $ column wor
        wor = wone x

showSecondWordpairCoords :: (Show a) => Wordpair a -> String
showSecondWordpairCoords x = lin ++ "," ++ col
  where lin = show $ line wor
        col = show $ column wor
        wor = wtwo x
