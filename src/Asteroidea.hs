module Asteroidea where

import Graphics.Gloss
-- import Graphics.Gloss.Raster.Field
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
  --playField window (1,1) fps initField getWorldPoint cap (updateField genRand 1)
  where
    colour = backGrCol
    fps = fpsMax
    window = InWindow "Just Nothing" (sizeX, sizeY) (startPosX, startPosY)
    -- FullScreen
    initField :: Field
    initField = createField sizeX sizeY
    cap :: a -> Field -> Field
    cap _ = id
{--
-- | Вывод поля на экран playField
getWorldPoint :: Field -> Point -> Color
getWorldPoint field (i,j) =
  unsafeGet trrI trrJ field
  where
    trrI = round $ halfSizeX + i*(fromIntegral sizeX)
    trrJ = round $ halfSizeY + j*(fromIntegral sizeY)
--}
-- | Вывод поля на экран simulate
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
    fishX i = (fromIntegral i-1) - (half sizeX)
    fishY j = (fromIntegral j-1) - (half sizeY)
    dl = 0.5