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

insertAt :: Int->a->[a]->[a]
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs

getSubTree :: VTree -> Path -> Maybe VTree
getSubTree vt [] = Just vt
getSubTree (Node _ l) (x:xs) | inRange l x =  getSubTree (l !! x) xs
                             | otherwise = Nothing
getSubTree (Leaf _) _ = Nothing

isValidTree :: VTree -> Bool
isValidTree (Node _ []) = False
isValidTree  (Leaf _) = True
isValidTree (Node _ list) = and (map isValidTree list) 

{-
insertSubTree :: Path->VTree->VTree->VTree
insertSubTree _ _ (Leaf v) = Leaf v
insertSubTree _ subT (Node op []) = Node op [subT]
insertSubTree [x] subt (Node op l) = Node op (insertAt x subt l)
insertSubTree (x:xs) subt (Node op l) | x < 0 = Node op (insertAt 0 (insertSubTree xs subt (l !! 0)) l) 
                                      | x >= length l = insertSubTree xs subt (l !! (length l - 1))
                                      | otherwise = insertSubTree xs subt (l !! x)
                                      -}