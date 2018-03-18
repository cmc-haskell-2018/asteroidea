module Const where
import Graphics.Gloss


-- | верхний порог числа бросков одной точки
innerIter :: Int
innerIter = 30
--1000
-- | нижний порог числа бросков точки, после которого начинается отрисовка
lowThreshold :: Int
lowThreshold = 20
-- | стартовый размер поля
-- не хочу рисковать лагами
-- 1920
sizeX :: Int
sizeX = 640
-- | половина поля, выраженная в вещественных значениях
half :: (Fractional a) => Int -> a
half size = (fromIntegral size)/2
-- | стартовый размер поля 
-- 1080
sizeY :: Int
sizeY = 360
-- | стартовая позиция окна
startPosX :: Int
startPosX = 0
-- | честно, ни малейшего понятия, будут ли здесь не нули
startPosY :: Int
startPosY = 0
-- | Фактор zoom.
zoom :: Float
zoom = 0.5*1000
backGrCol :: Color
-- ^ Цвет заднего фона
backGrCol = black
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