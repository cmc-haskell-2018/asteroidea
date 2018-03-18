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
--simulate window colour fps initField imageScan (updateField genRand)
--colour = backGrCol
  playField window (1,1) fps (initWorld genRand) getWorldPoint cap updateWorld
  where
    fps = fpsMax
    window = InWindow "Just Nothing" (sizeX, sizeY) (startPosX, startPosY)
-- FullScreen
initWorld :: StdGen -> World
-- ^ создание мира
initWorld sgen = World (createField sizeX sizeY) sgen busPointList
-- | заглушка на месте обработки событий
cap :: a -> World -> World
cap _ = id

-- | Вывод поля на экран playField
getWorldPoint :: World -> Point -> Color
getWorldPoint bnw (i,j) =
  getElem trrI trrJ (field bnw)
  where
    trrI = round ((i+1)*(half sizeX) ) + 1
    trrJ = round ((j+1)*(half sizeY) ) + 1

{--
-- | Вывод поля на экран simulate
imageScan :: Field -> Picture
imageScan field =
  pictures $ concat
  [
    [ -- ^ Покраска в цвет точки,
      Color
      -- ^ не контролируя выход за границы массива,
      (unsafeGet i j field) $
      -- ^ квадрата, покрывающего данную точку
      Polygon $
      (getNeigbours dl (fishX i, fishY j))
      | i <- [1..sizeX]
    ] 
    | j <- [1..sizeY]
  ]
  where
    fishX i = (fromIntegral i-1) - (half sizeX)
    fishY j = (fromIntegral j-1) - (half sizeY)
    dl = 0.5
--}

-- | Cast Infinite List
busPointList :: [Cast]
busPointList = 
  [(point, colC) | point <- biUnitTiling]
  where
    colC = 0.5
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
-- | бесконечный список соседей
biUnitTiling :: [Vec]
biUnitTiling = concat [ nthNeigbours i | i <- [0,1..]]
