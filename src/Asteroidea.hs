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
{--
-- | Запуск симуляции gloss-raster playField, динамика
run :: IO ()
run = do 
  genRand <- newStdGen
  playField window (1,1) fps (initWorld genRand) getter cap update
  where
    fps = fpsMax
    getter = getWorldPoint
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
    update = updateWorld
--}
-- | Запуск симуляции gloss display через gloss-raster, статика
run :: IO()
run = do
  genRand <- newStdGen
  display window backGrCol (picture genRand)
  where
    getter g = getWorldPoint' $ updateWorld mainIter $ initWorld g
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
    picture g = makePicture winX winY 1 1 (getter g)

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
getWorldPoint'
  :: World -- ^ World
  -> Point -- ^ Point from [-1,1]^2 conformal mapping to Field
  -> Color -- ^ safe Color from Field
{-
getWorldPoint bnw (x,y)
  | flag = mkCol $ getElem i j (mugenga bnw)
  | otherwise = backGrCol
  where
    x',y' :: Float -- ^ Orthogonal transformation (x,y)
    x' = y*sinTheta + x*cosTheta
    y' = y*cosTheta - x*sinTheta
    i, j :: Int -- ^ Translation on shift vector in discrete field
    i = round (x' * halfX - shiftX)
    j = round (y' * halfY - shiftY)
    flag :: Bool -- ^ Control bounds of field, flag
    flag = not (cond sizeX i || cond sizeY j)
    cond size a = a < 1 || a > size
-}
getWorldPoint' bnw (x,y) = mkCol $ getElem i j (mugenga bnw)
  where
    i = round (x * halfX - shiftX)
    j = round (y * halfY - shiftY)
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
