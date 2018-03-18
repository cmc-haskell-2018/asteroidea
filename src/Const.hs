module Const where
import Graphics.Gloss

innerIter :: Int
innerIter = 30
--1000
lowThreshold :: Int
lowThreshold = 20
sizeX :: Int
sizeX = 640
-- 1920
half :: (Fractional a) => Int -> a
half size = (fromIntegral size)/2

sizeY :: Int
sizeY = 360
-- 1080

startPosX :: Int
startPosX = 0
startPosY :: Int
startPosY = 0
{--
half :: (Fractional a) => Int -> a
half size = (fromIntegral sizeX)/2
--}
zoom :: Float
zoom = 0.5*1000
backGrCol :: Color
-- Цвет заднего фона
backGrCol = black
fpsMax :: Int
fpsMax = 1
numCast :: Float
numCast = 1000
--100000
modelCount :: Double
modelCount = 3