module Const where
import Graphics.Gloss

innerIter :: Int
innerIter = 1
--1000
lowThreshold :: Int
lowThreshold = 20
sizeX :: Int
sizeX = 1920
sizeY :: Int
sizeY = 1080
startPosX :: Int
startPosX = 0
startPosY :: Int
startPosY = 0
zoom :: Float
zoom = 0.5*1000
outerIter :: Int
outerIter = 80000 
--Поглощено NumCast
backGrCol :: Color
-- Цвет заднего фона
backGrCol = black
fpsMax :: Int
fpsMax = 1
--30
numCast :: Float
numCast = 1000
--100000
modelCount :: Double
modelCount = 3
