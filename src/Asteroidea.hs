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

import Asteroidea.ClassField
import Asteroidea.Const
import Asteroidea.Types

-- | Поехали!
-- Генератор случайных чисел, начальная инициализация
-- Запуск симуляции Gloss - ради получения стартового интерфейса:
-- масштабирование и передвижение
-- см. 'World' в ClassField
run :: IO ()
run = do 
  genRand <- newStdGen
  simulate displayW colour fps (initWorld genRand) imageScan update
  where
    displayW = InWindow mainName (sizeX, sizeY) (startPosX, startPosY)
    -- FullScreen NOT ADVISABLE
    colour = backGrCol -- Цвет фона не поля, но окна.
    fps = fpsMax -- frames per second
    update = \_ -> updateWorld

-- | проход по картинке, возвращающий картинку 'Picture'
{- uses 'makePicture' from Graphics.Gloss.Raster.Field
   makePicture
        :: Int                  -- Window Size X
        -> Int                  -- Window Size Y
        -> Int                  -- Pixels X, coefficient
        -> Int                  -- Pixels Y, coefficient
        -> (Point -> Color)     -- 'getWorldPoint' :: World -> Point -> Color
        -> Picture
-}
imageScan :: World -> Picture
imageScan bnw = makePicture sizeX sizeY 1 1 (getWorldPoint bnw)

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
-- 'unsafeGet' :: Int-> Int -> Matrix a -> a
-- без проверки на соответствие границ, import from 'Matrix'
getWorldPoint
  :: World -- ^ World
  -> Point -- ^ Point from [-1,1]^2 conformal mapping to Field
  -> Color -- ^ safe Color from Field
getWorldPoint bnw
  = \(x,y) -> let
      i, j :: Int -- ^ Translation on shift vector in discrete field
      i = round ((x+1) * halfX) +1
      j = round ((y+1) * halfY) +1
    in mkCol $! unsafeGet i j field
  where field = mugenga bnw
      
mkCol :: UnsafeColour -> Color
-- ^ convertation with log scale and checking
mkCol (r,g,b,a) = rgb' (control r) (control g) (control b)
  where
    -- better choice is log1p, but it is not accessible
    logscale = (log a)/a
    control x = normal $ logscale * x
    -- normalization to [0,1]
    normal sample
      | sample > 1.0 = 1.0
      | otherwise = sample

{- | соседи одной точки, расположенные в центрах окружающих квадратов
порядок обхода - по контуру
v22  -  v11
 |  [0]  |
v21  -  v12
-}
getNeigbours
  :: Double -- ^ size
  -> Vec    -- ^ center
  -> [Vec]
getNeigbours dl (x,y) = [v11,v12,v21,v22]
  where
    v11 = (x+dl,y+dl)
    v12 = (x+dl,y-dl)
    v21 = (x-dl,y-dl)
    v22 = (x-dl,y+dl)
-- | Список соседей одного порядка.
-- геометрическая прогрессия: 1, 4, 16, 64 ..
-- использует 'getNeigbours' и рекурсивное обращение к себе
nthNeigbours :: Int -> [Vec]
nthNeigbours n | n>0 = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2 ** (- fromIntegral n)
nthNeigbours _ = [(0,0)]
-- | Cast Infinite List
-- бесконечный список соседей
-- использует 'nthNeigbours'
busPointList :: [Vec]
busPointList = concat [ nthNeigbours i | i <- [0,1..]]
