{-|
Module      : ClassField
Description : operations with field, generating invocation to Model
Copyright   : Just Nothing
Stability   : in progress
-}
module ClassField where

import Graphics.Gloss()
import System.Random
import Data.Matrix
import Types
import Const
import GVector()
import Transform
-- | Обёртка над Field, играющая роль мира. Без грязного IO.
data World =
  World {
    mugenga :: !Field,  -- ^ 無限画
    getSGen :: !StdGen, -- ^ standart pseudorandom number generator
    busList :: [Vec]  -- ^ BiUnitSquare coverage list
        }
-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)
-- | Бросок с привязанным генератором
type CastGen = (GVec,Double)
-- | Поле есть матрица цветов
type Field = Matrix UnsafeColour
-- | Цвета без нормализации и проверки на нормировку
type UnsafeColour = (Float,Float,Float,Float)
-- | Создание изначального поля
-- размером икс на игрек
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
-- | Начальное заполнение фона
-- -> function from field point to unsafe colour
initFunction
  :: Int -- ^ width
  -> Int -- ^ heigth
  -> ((Int,Int)->UnsafeColour)
initFunction _ _ =
  ( \_ -> (0.13,0.54,0.13,1.0))
{-| ^ веселья ради можно поставить что-то ещё,
 но цвет лесной зелени приятен глазу, как ветви молодых деревьев в летнем саду.
-}

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
updateWorld
  :: Float -- ^ delta time
  -> World -- ^ Old World
  -> World -- ^ New World
updateWorld dt bnw =
  bnw `seq`
  generator
  bnw
  $ floor (numCast) --(dt*NumCast)

-- | генератор нового поля
-- генерируется новая серия бросков одной точки из bus
-- в каждой итерации, по счётчику с декрементом
generator
  :: World -- ^ BNW
  -> Int   -- ^ iterator for loop
  -> World
generator bnw n | n>0 = bnw `seq` rty (iter (mugenga bnw, busPoint $! bnw) 0) (n-1)
  where
    rty (f,(gvec,_)) = generator (World f (gvGen gvec) $ tail . busList $ bnw)
generator a _  = a

-- | BiUnitSquarePoint  from [-1,1)^2
-- with colour 0.5
busPoint :: World -> CastGen
busPoint bnw = bnw `seq` (GVec (getSGen bnw) (head $ busList bnw), 0.5)

-- | Iterator for loop inner_iter
-- new Field, new PRNG
-- броски одной точки
-- итерации по счётчику с инкрементом
-- начало отрисовки с нижнего порога
-- конец отрисовки на верхнем пороге
-- pack - упаковка результата с отисовкой в поле
iter
  :: (Field, CastGen) -- ^ old field
  -> Int              -- ^ counter
  -> (Field, CastGen) -- ^ new field
iter (f, cgen) n
  | n<lowThreshold = cgen `seq` iter (f,(newCast cgen)) (n+1)
  | n<innerIter = f `seq` iter (pack (newCast cgen)) (n+1)
  | otherwise = (f, cgen)
  where
    pack newC@(gvec, colour) = ((plot (gvVec gvec, colour) f), newC)

-- | TODO Генерация новой точки
newCast :: CastGen -> CastGen
newCast (gvector, colour) =
  let
    (choice,generator) = random $ gvGen gvector
    transform = askTransform mainModel choice
  in applyTransform transform colour (GVec generator (gvVec gvector))

-- | Размещение точки в поле
plot
  :: Cast  -- ^ cast: (point, gradient)
  -> Field -- ^ old field
  -> Field -- ^ new field
plot ((x, y), colC) field
  | flag = -- размещение
    setElem colour coord field
  | otherwise = -- выход за границы
    field
  where
    colour = merge colC $ getPoint coord -- слияние с точкой на месте
    getPoint (a,b) = getElem a b field -- получение текущего состояния
    flag = control (x', y') -- флаг выхода за границы
-- Orthogonal transformation (x,y)
    x' = ((y*sinTheta + x*cosTheta) - shiftX)
    y' = ((y*cosTheta - x*sinTheta) - shiftY)
-- Translation on shift vector in discrete field and scaling
    ordX = x' + halfX + 1
    ordY = y' + halfY + 1
-- Integer coordinates x y
    coord = (trr ordX, trr ordY)
    trr = floor
-- | проверка границ поля
control :: (Double,Double) -> Bool
control (a,b) = not (cond halfX a || cond halfY b)
  where
    cond size x =
      isNaN x ||
      isInfinite x ||
      x < - size ||
      x >   size
-- | TODO alpha blending colours
merge :: Double -> UnsafeColour -> UnsafeColour
merge grad col = mix ((gradient mainModel)!!(floor(sumGrad*grad))) col -- asking gradient
  where
    mix (r,g,b,_) (t,h,n,s) = (r+t,g+h,b+n,s+1)