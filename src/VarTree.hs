{-|
Module      : VarTree
Description : operations with Expression Tree of Variations
Copyright   : Just Nothing
Stability   : in progress
-}
module VarTree where
import Prelude  
import System.Random
import Types

(!!!) :: [a]->Int->Maybe a
(!!!) l i
         | i < 0 = Nothing
         | i >=length l = Nothing
         | otherwise = Just $ l !! i

type Operation = GVec->GVec->GVec
type Path = [Int]
data VTree = Node Operation [VTree] | Leaf Variation

getSubTree :: VTree -> Path -> Maybe VTree
getSubTree vt [] = Just vt
getSubTree (Node op l) (x:xs) | l !!! x == Nothing = Nothing 
                              | otherwise = getSubTree (l !!! x) xs
getSubTree (Leaf _) _ = Nothing