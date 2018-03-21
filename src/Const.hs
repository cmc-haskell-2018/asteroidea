{-|
Module      : Const
Description : Parameters for program and convertation functions
Copyright   : Just Nothing
Stability   : in progress
-}
module Const where
import Examples (exampleModel)
import Graphics.Gloss.Data.Color (Color,makeColor)
import Types (Model,width,height,mScale,rotation)
import Codec.Picture
-- | export example model
mainModel :: Model
mainModel = exampleModel
{-
-- | смещение центра фрактала по оси абсцисс 
shiftX :: Float
shiftX = -1 - halfX
-- | смещение центра фрактала по оси ординат
shiftY :: Float
shiftY = -1 - halfY
-}
-- | rotation in radian
rotRad :: Double
rotRad = (pi/360*) $ rotation mainModel
-- | sin rotation
sinTheta :: Float
sinTheta = realToFrac . (/scaleFactor) $ (sin rotRad)
-- | cos rotation
cosTheta :: Float
cosTheta = realToFrac . (/scaleFactor) $ (cos rotRad)
-- | Scale Factor
scaleFactor :: Double
scaleFactor = (mScale mainModel) / 50
{- Zoom Factor, scaling
zoomFactor :: Double
zoomFactor = exp
-}
-- | Цвет заднего фона

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