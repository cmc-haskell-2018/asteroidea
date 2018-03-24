{-|
Module      : Asteroidea
Description : Main module, starting simulation Αστερίας
Copyright   : Just Nothing
Stability   : in progress
-}
module Asteroidea where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Raster.Field
import Data.Matrix

import Const
import Types
import ClassField

-- | Поехали!
-- Генератор случайных чисел, начальная инициализация
-- Запуск симуляции Gloss - ради получения стартового интерфейса:
-- масштабирование и передвижение
run :: IO ()
run = do 
  genRand <- newStdGen
  simulate displayW colour fps (initWorld genRand) imageScan update
  where
    displayW = InWindow mainName (sizeX, sizeY) (startPosX, startPosY)
    -- FullScreen
    colour = backGrCol
    fps = fpsMax
    -- too slow, too slow!
    imageScan :: World -> Picture
    imageScan bnw = makePicture sizeX sizeY 1 1 (getWorldPoint bnw)
    update = \_ -> updateWorld

{--
-- Запуск параллельной версии play библиотекой gloss-raster
-- Возможно, GUI будем писать именно здесь
run :: IO ()
run = do 
  genRand <- newStdGen
  playField window (1,1) fps (initWorld genRand) getter cap update
  where
    fps = fpsMax
    getter = getWorldPoint
    window = InWindow mainName (winX, winY) (startPosX, startPosY)
    update = updateWorld
-- | заглушка на месте обработки событий
cap :: a -> World -> World
cap _ = id
--}

-- | Act of Creation
-- создание мира
initWorld
  :: StdGen -- ^ Глобальный ГПСЧ мира, получен со старта.
  -> World
initWorld sGen = World (createField sizeX sizeY) sGen busPointList


-- | Вывод поля на экран playField
getWorldPoint
  :: World -- ^ World
  -> Point -- ^ Point from [-1,1]^2 conformal mapping to Field
  -> Color -- ^ safe Color from Field
getWorldPoint !bnw (x,y)
  = mkCol $! getElem i j (mugenga bnw)
  where
    i, j :: Int -- ^ Translation on shift vector in discrete field
    i = round ((x+1) * halfX) +1
    j = round ((y+1) * halfY) +1
mkCol :: UnsafeColour -> Color
-- ^ convertation with log scale and checking
mkCol (r,g,b,a) = rgb' (control r) (control g) (control b)
  where
    -- better choice is log1p, but it is not accessible
    logscale = (*) . (/a) $ log $ 1+a
    control x = normal $ logscale x
    -- normalization to [0,1)
    normal sample
      | sample > 1.0 = 1.0
      | otherwise = sample

{- | соседи одной точки, расположенные в центрах окружающих квадратов
порядок обхода - по контуру
v22  -  v11
 |  [0]  |
v21  -  v12
-}
getNeigbours :: Double -> Vec -> [Vec]
getNeigbours dl (x,y) = [v11,v12,v21,v22]
  where
    v11 = (x+dl,y+dl)
    v12 = (x+dl,y-dl)
    v21 = (x-dl,y-dl)
    v22 = (x-dl,y+dl)
-- | Список соседей одного порядка.
-- геометрическая прогрессия: 1, 4, 16, 64 ..

nthNeigbours :: Int -> [Vec]
nthNeigbours n | n>0 = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2 ** (- fromIntegral n)
nthNeigbours _ = [(0,0)]
-- | Cast Infinite List
-- | бесконечный список соседей
busPointList :: [Vec]
busPointList = concat [ nthNeigbours i | i <- [0,1..]]