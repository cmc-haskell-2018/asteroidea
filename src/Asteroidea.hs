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
    window = InWindow "Just Nothing" (sizeX, sizeY) (startPosX, startPosY)
    -- FullScreen
    colour = backGrCol
    fps = fpsMax
    initField :: Field
    initField = createField sizeX sizeY
    -- | Вывод поля на экран
    imageScan :: Field -> Picture
    imageScan field =
      pictures $ concat
      [
        [ Color
          (unsafeGet i j field) $
          Line [(fromIntegral i,fromIntegral j)]
          | i <- [1..sizeX]
        ] 
        | j <- [1..sizeY]
      ]