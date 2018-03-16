module Asteroidea where

import Graphics.Gloss
import ClassField
import System.Random
import Const
import Data.Matrix
import Types

run :: IO ()
run = do
-- | Генератор случайных чисел, начальная инициализация
  genRand <- newStdGen
-- | Запуск симуляции
  simulate window colour fps initField imageScan (updateField genRand) 
  where
    colour = backGrCol
    fps = fpsMax
    window = InWindow "Just Nothing" (sizeX, sizeY) (startPosX, startPosY)
    -- FullScreen
    initField :: Field
    initField = createField sizeX sizeY
-- | Вывод поля на экран
imageScan :: Field -> Picture
imageScan field =
  pictures $ concat
  [
    [ -- | Покраска в цвет точки,
      Color
      -- | не контролируя выход за границы массива,
      (unsafeGet i j field) $
      -- | квадрата, покрывающего данную точку
      Polygon $
      (getNeigbours dl (fishX i, fishY j))
      | i <- [1..sizeX]
    ] 
    | j <- [1..sizeY]
  ]
  where
    fishX i = (fromIntegral i)-halfSizeX
    fishY j = (fromIntegral j)-halfSizeY
    dl = 0.5