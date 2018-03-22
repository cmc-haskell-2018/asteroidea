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

-- | Дерево выражения над вариациями
-- | Позволяет задать произвольное выражение из вариаций,
-- | и при этом иметь доступ к любым их параметрам и структуре выражения
-- | что необходимо для работы скриптов и изменения модели через гуи
data VTree = Node Operation [VTree] | Leaf Variation


insertAt :: Int->a->[a]->[a]
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs

-- | получить поддерево по заданной последовательности переходов по вершинам
getSubTree :: VTree -> Path -> Maybe VTree
getSubTree vt [] = Just vt
getSubTree (Node _ l) (x:xs) | inRange l x =  getSubTree (l !! x) xs
                             | otherwise = Nothing
getSubTree (Leaf _) _ = Nothing

isValidTree :: VTree -> Bool
isValidTree (Node _ []) = False
isValidTree  (Leaf _) = True
isValidTree (Node _ list) = and (map isValidTree list) 

--removeSubTree :: VTree -> Path -> VTree -- откуда \ по какому адресу удалять
--replaceSubTree :: VTree->Path->VTree->VTree -- откуда \ по какому адресу \ чем заменить 
-- | Свёртка дерева в функцию из G-вектора в G-вектор
foldTree :: VTree -> GVec -> GVec
foldTree (Leaf var) gvec = (vScale var) |*| ((function var) (params var) gvec)
foldTree (Node op list) gvec = foldr f' (head listGVec) (tail listGVec)
  where
--  aka foldr'
    f' x z = id $! (op z x) 
-- listGVec = zipWith (VTree -> GVec -> GVec) [VTree] [GVec] -> [GVec]
-- right-lazy zipWith f [] _|_ = []
    listGVec = zipWith (\tree vec -> (foldTree tree) vec) list (listSplit gvec)
-- GVec list
    listSplit (GVec sgen vec)
      = [(GVec newgen vec) | newgen <- listgen sgen]
-- ^ PRNG list
    listgen gen0
      = gen1 : listgen gen2
      where (gen1,gen2) = split gen0

{-
insertSubTree :: Path->VTree->VTree->VTree
insertSubTree _ _ (Leaf v) = Leaf v
insertSubTree _ subT (Node op []) = Node op [subT]
insertSubTree [x] subt (Node op l) = Node op (insertAt x subt l)
insertSubTree (x:xs) subt (Node op l) | x < 0 = Node op (insertAt 0 (insertSubTree xs subt (l !! 0)) l) 
                                      | x >= length l = insertSubTree xs subt (l !! (length l - 1))
                                      | otherwise = insertSubTree xs subt (l !! x)
                                      -}