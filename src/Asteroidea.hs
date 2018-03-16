module Asteroidea where

import Graphics.Gloss
import ClassField
import System.Random
import Const
import Data.Matrix

run :: IO ()
run = do
-- | Генератор случайных чисел, начальная инициализация
  genRand <- newStdGen
-- | Запуск симуляции
  simulate window colour fps initField imageScan (updateField genRand) 
  where
    colour = backGrCol
    fps = fpsMax
--updateCap _ _ = id
window = InWindow "Just Nothing" (sizeX, sizeY) (startPosX, startPosY)
-- FullScreen
initField :: Field
initField = createField sizeX sizeY
-- | Вывод поля на экран
imageScan :: Field -> Picture
imageScan field =
  pictures $ concat
  [
    [ Color
      (unsafeGet i j field) $
      Polygon
        [
          (fihsX i - 0.5, fihsY j - 0.5) ,
          (fihsX i + 0.5, fihsY j - 0.5) ,
          (fihsX i + 0.5, fihsY j + 0.5) ,
          (fihsX i - 0.5, fihsY j + 0.5)
        ]
      | i <- [1..sizeX]
    ] 
    | j <- [1..sizeY]
  ]
  where
    fihsX i = (fromIntegral i)-halfSizeX
    fihsY j = (fromIntegral j)-halfSizeY