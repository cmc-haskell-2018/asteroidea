{-|
Module      : Asteroidea
Description : Main module, starting simulation
Copyright   : Just Nothing
Stability   : in progress
-}
module Asteroidea where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Raster.Field
import Data.Matrix

import Const
import Types
import ClassField

-- | Поехали!
run :: IO ()
run = do 
-- Генератор случайных чисел, начальная инициализация
  genRand <- newStdGen
-- Запуск симуляции
  playField window (1,1) fps (initWorld genRand) getter cap update
  where
    fps = fpsMax
    getter = getWorldPoint
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
    update = updateWorld
-- window -- FullScreen

-- | Act of Creation
-- создание мира
initWorld :: StdGen -> World
initWorld sGen = World (createField sizeX sizeY) sGen busPointList
-- | заглушка на месте обработки событий
cap :: a -> World -> World
cap _ = id

-- | Вывод поля на экран playField
getWorldPoint :: World -> Point -> Color
getWorldPoint bnw (i,j)
  | flag = getElem trrI trrJ (mugenga bnw)
  | otherwise = backGrCol
  where
    trrI = round ((x+1)*(half sizeX) ) + 1
    trrJ = round ((y+1)*(half sizeY) ) + 1
    x = j*sinTheta + i*cosTheta
    y = j*cosTheta - i*sinTheta
    flag = not (cond sizeX trrI|| cond sizeY trrJ)
    cond size a = a < 1 || a > size

-- | соседи одной точки, расположенные в центрах окружающих квадратов
-- порядок обхода - по контуру
-- полиморф только из-за повторного использования в Gloss simulate
-- можно восстановить getNeigbours :: Double -> Vec -> [Vec]
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
