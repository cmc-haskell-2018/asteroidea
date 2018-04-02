{-|
Module      : Const
Description : Parameters for program and convertation functions
Copyright   : Just Nothing
Stability   : in progress
-}
module Const where

-- 1920
winX :: Int
winX = 1024
-- | стартовый размер окна 
-- 1080
winY :: Int
winY = 576
-- | стартовая позиция окна
startPosX :: Int
startPosX = 0
-- | честно, ни малейшего понятия, будут ли здесь не нули
startPosY :: Int
startPosY = 0

{-
-- | Функции для и само замощение квадрата [-1,1]^2
getNeigbours::Num a =>  a->(a,a)->[(a,a)]
getNeigbours dl (x,y) = [v11,v12,v22,v21]
  where
    v11 = (x+dl,y+dl)
    v12 = (x+dl,y-dl)
    v21 = (x-dl,y+dl)
    v22 = (x-dl,y-dl)
-- | список соседей одного порядка
nthNeigbours :: Int -> [Vec]
nthNeigbours n | n>0 = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2 ** (- fromIntegral n)
nthNeigbours _ = [(0,0)]
-- | Cast Infinite List
-- | бесконечный список соседей
busPointList :: [Vec]
busPointList = concat [ nthNeigbours i | i <- [0,1..]]
-}