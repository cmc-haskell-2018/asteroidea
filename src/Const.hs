{-|
Module      : Const
Description : Parameters for program and convertation functions
Copyright   : Just Nothing
Stability   : in progress
-}
module Const where
import Examples (exampleModel)
import Graphics.Gloss.Data.Color (Color,makeColor)
import Types (Model,width,height,mScale,rotation,weight,transforms,gradient)
-- | export example model
mainModel :: Model
mainModel = exampleModel
-- | x, y size of field, model, window, etc
sizeX,sizeY :: Int
sizeX = width  mainModel
sizeY = height mainModel
-- | Суммарный вес всех трансформ в вероятностном распределении
sumWeight::Double
sumWeight = foldr (+) 0 (map weight (transforms mainModel))
-- | общее число цветов в карте градиентов
sumGrad :: Double
sumGrad = fromIntegral $ length (gradient mainModel)
-- | смещение центра фрактала по оси абсцисс / ординат
shiftX, shiftY :: Double
shiftX = 0
shiftY = 0
-- | rotation in radian
rotRad :: Double
rotRad = (pi/360*) $ rotation mainModel
-- | sin / cos rotation
sinTheta, cosTheta :: Double
sinTheta = realToFrac . (/scaleFactor) $ (sin rotRad)
cosTheta = realToFrac . (/scaleFactor) $ (cos rotRad)
-- | Scale Factor
scaleFactor :: Double
scaleFactor = 1/(mScale mainModel)
{- Zoom Factor, scaling
zoomFactor :: Double
zoomFactor = exp
-}
-- | Цвет заднего фона
backGrCol :: Color
backGrCol = makeColor 0 0 0 1
-- | верхний порог числа бросков одной точки
innerIter :: Int
innerIter = 128+64
--1000
-- | нижний порог числа бросков точки, после которого начинается отрисовка
lowThreshold :: Int
lowThreshold = 24
-- | стартовый размер окна
-- не хочу рисковать лагами
-- 1920 x 1080
winX, winY :: Int
winX = sizeX
winY = sizeY
-- | половина поля, выраженная в вещественных значениях
halfX, halfY :: (Fractional a) => a
halfX = (fromIntegral sizeX)/2
halfY = (fromIntegral sizeY)/2
-- | стартовая позиция окна
-- честно, ни малейшего понятия, будут ли здесь не нули
startPosX, startPosY :: Int
startPosX = 0
startPosY = 0
-- | максимальная частота кадров.
-- единица это минимум
fpsMax :: Int
fpsMax = 1
-- | число бросков из BiUnitSquare за шаг отрисовки
-- gloss-raster, похоже, даёт время в секундах, но в этом не уверен
-- 100000
numCast :: Int
numCast = 1