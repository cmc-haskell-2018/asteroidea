{-|
Module      : Const
Description : Parameters for program and convertation functions
Copyright   : Just Nothing
Stability   : in progress
-}
module Const where
import Variation (mainModel)
import Graphics.Gloss.Data.Color (Color,makeColor)
import Types (width,height,mScale,rotation)
-- | x size of field, model, window, etc
sizeX :: Int
sizeX = width  mainModel
-- | y size of field, model, window, etc
sizeY :: Int
sizeY = height mainModel
-- | rotation in radian
rotRad :: Double
rotRad = (pi/360*) $ rotation mainModel
-- | sin rotation
sinTheta :: Float
sinTheta = realToFrac . (/zoomFactor) $ (sin rotRad)
-- | cos rotation
cosTheta :: Float
cosTheta = realToFrac . (/zoomFactor) $ (cos rotRad)
-- | Zoom Factor, scaling
zoomFactor :: Double
zoomFactor = exp . mScale $ mainModel
-- | Цвет заднего фона
backGrCol :: Color
backGrCol = makeColor 0 0 0 1
-- | верхний порог числа бросков одной точки
innerIter :: Int
innerIter = 30
--1000
-- | нижний порог числа бросков точки, после которого начинается отрисовка
lowThreshold :: Int
lowThreshold = 20
-- | стартовый размер окна
-- не хочу рисковать лагами
-- 1920
winX :: Int
winX = 640
-- | половина поля, выраженная в вещественных значениях
half :: (Fractional a) => Int -> a
half size = (fromIntegral size)/2
-- | стартовый размер окна 
-- 1080
winY :: Int
winY = 360
-- | стартовая позиция окна
startPosX :: Int
startPosX = 0
-- | честно, ни малейшего понятия, будут ли здесь не нули
startPosY :: Int
startPosY = 0
-- | Фактор zoom.
zoom :: Float
zoom = 0.5*1000
fpsMax :: Int
-- ^ максимальная частота кадров.
-- единица это минимум
fpsMax = 1
-- | число бросков из BiUnitSquare за единицу времени
-- gloss-raster, похоже, даёт время в секундах, но в этом не уверен
-- 100000
numCast :: Float
numCast = 10
-- | В целях отладки - число моделей Transform в фрактале Model
modelCount :: Double
modelCount = 3