module Const where
import Graphics.Gloss

innerIter :: Int
innerIter = 1
--1000
lowThreshold :: Int
lowThreshold = 20
sizeX :: Int
sizeX = 640
-- 1920
halfSizeX :: Float
halfSizeX = fromIntegral sizeX/2
halfXD :: Double
halfXD = fromIntegral sizeX/2
sizeY :: Int
sizeY = 360
-- 1080
halfSizeY :: Float
halfSizeY = fromIntegral sizeY/2
halfYD :: Double
halfYD = fromIntegral sizeY/2
startPosX :: Int
startPosX = 0
startPosY :: Int
startPosY = 0
zoom :: Float
zoom = 0.5*1000
backGrCol :: Color
-- Цвет заднего фона
backGrCol = black
fpsMax :: Int
fpsMax = 30
numCast :: Float
numCast = 1000
--100000
modelCount :: Double
modelCount = 3
