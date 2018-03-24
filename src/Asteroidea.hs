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
-- Запуск симуляции

run :: IO ()
run = do 
  genRand <- newStdGen
  playField window (1,1) fps (initWorld genRand) getter cap update
  where
    fps = fpsMax
    getter = getWorldPoint
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
    update = updateWorld

-- | Act of Creation
-- создание мира
initWorld
  :: StdGen -- ^ Глобальный ГПСЧ мира, получен со старта.
  -> World
initWorld sGen = World (createField sizeX sizeY) sGen busPointList
-- | заглушка на месте обработки событий
cap :: a -> World -> World
cap _ = id

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
mkCol (r,g,b,a) = rgb' (cf r) (cf g) (cf b)
  where cf = (*) . (/a) $ log $ 1+a

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