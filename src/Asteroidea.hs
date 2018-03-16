module Asteroidea where

import Graphics.Gloss
import ClassField
import System.Random
import Const
import Data.Matrix

run :: IO ()
run = do
-- | Генератор случайных чисел, начальная инииализация
  genRand <- newStdGen
-- | Запуск симуляции
  simulate display color fps initField imageScan (updateField genRand) 
  where
    -- | окно
    display = InWindow "Just Nothing" (sizeX, sizeY) (startPosX, startPosY)
    -- FullScreen
    color = backGrCol
    fps = fpsMax
    initField :: Field
    initField = createField sizeX sizeY
    imageScan :: Field -> Picture
    imageScan field =
      pictures (concat
      [[ Color (unsafeGet i j field) (Line [(fromIntegral i,fromIntegral j),(fromIntegral i,fromIntegral j)]) | i<- [1..sizeX]] | j <- [1..sizeY]])