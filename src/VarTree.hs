{-|
Module      : VarTree
Description : operations with Expression Tree of Variations
Copyright   : Just Nothing
Stability   : in progress
-}
module VarTree where
import Prelude  
--import System.Random
import Types

inRange :: [a]->Int->Bool
inRange l i
         | i < 0 = False
         | i >=length l = False
         | otherwise = True

type Operation = GVec->GVec->GVec
type Path = [Int]
data VTree = Node Operation [VTree] | Leaf Variation

getSubTree :: VTree -> Path -> Maybe VTree
getSubTree vt [] = Just vt
getSubTree (Node _ l) (x:xs) | inRange l x =  getSubTree (l !! x) xs
                             | otherwise = Nothing
getSubTree (Leaf _) _ = Nothing