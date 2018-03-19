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
import GVector

type Operation = GVec->GVec->GVec
type Path = [Int]
data VTree = Node Operation [(Int,VTree)] | Leaf Variation