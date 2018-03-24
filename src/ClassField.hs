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
    mugenga :: !Field,  -- ^ 無限画 Main Field, main model parameterized
    getSGen :: !StdGen, -- ^ standart pseudorandom number generator
    busList :: [Vec]    -- ^ BiUnitSquare coverage infinite list
        }
-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)
-- | Бросок с привязанным генератором
type CastGen = (GVec,Double)
-- | Поле есть матрица цветов
type Field = Matrix UnsafeColour

-- | Создание изначального поля
-- размером икс на игрек
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
-- | Начальное заполнение фона
initFunction
  :: Int -- ^ width
  -> Int -- ^ heigth
  -> ((Int,Int)->UnsafeColour)
  -- ^ function from field point to unsafe colour
initFunction _ _ =
  \_ -> (0,0,0,1)
  --( \_ -> (0.13,0.54,0.13,1.0))
{-| ^ веселья ради можно поставить что-то ещё,
 но цвет лесной зелени приятен глазу, как ветви молодых деревьев в летнем саду.
  (\(a,b) ->
    (
      fromIntegral (a `div` 5 + 1) /255,
      fromIntegral (140 - a `div` 54 - b `div` 32) /255,
      fromIntegral (34 + b `div` 5) /255,
      1.0
    )
  )
-}

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
updateWorld
  :: Float -- ^ delta time
  -> World -- ^ Old World
  -> World -- ^ New World
updateWorld dt bnw =
  bnw `seq`
  generator bnw
  $ floor (numCast) --(dt*NumCast)

-- | генератор нового поля
-- генерируется новая серия бросков одной точки из bus
-- в каждой итерации, по счётчику с декрементом
generator
  :: World -- ^ Brave New World
  -> Int   -- ^ counter for loop
  -> World
generator bnw n | n>0 =
  bnw `seq` rty (iter (mugenga bnw, busPoint $! bnw) 0) (n-1)
  where
    -- repacking and cutting head of BUS list
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
-- pack - упаковка результата с отрисовкой в поле
iter
  :: (Field, CastGen) -- ^ old field
  -> Int              -- ^ counter
  -> (Field, CastGen) -- ^ new field
iter (f, cgen) n
  -- если ниже первого порога - бросаем дальше
  | n<lowThreshold = cgen `seq` iter (f,(newCast cgen)) (n+1)
  -- если выше - рисуем на поле и бросаем дальше
  | n<innerIter = f `seq` iter (pack (newCast cgen)) (n+1)
  -- конец цикла
  | otherwise = (f, cgen)
  where
    pack newC@(gvec, colour) = ((plot (gvVec gvec, colour) f), newC)

-- | Генерация новой точки
newCast :: CastGen -> CastGen
newCast (gvector, colour) =
  let
    -- выборка случайной величины [0,1)
    (choice,generator) = random $ gvGen gvector
    -- выбор соответствующей ей трансформы
    transform = askTransform mainModel choice
  -- применение трансформы к вектору
  in applyTransform transform colour (GVec generator (gvVec gvector))

-- | Размещение точки в поле
plot
  :: Cast  -- ^ cast: (point, gradient)
  -> Field -- ^ old field
  -> Field -- ^ new field
plot ((x, y), colC) field
  | flag = -- размещение в пределах границ
    setElem colour coord field
  | otherwise = -- выход за границы
    field
  where
    colour = merge colC $ getPoint coord -- слияние с точкой на месте
    getPoint (a,b) = getElem a b field -- получение текущего состояния
    flag = controlBounds (x', y') -- флаг выхода за границы
    -- Orthogonal transformation (x,y) and scaling @sinTheta@
    x' = ((y*sinTheta + x*cosTheta) - shiftX)
    y' = ((y*cosTheta - x*sinTheta) - shiftY)
    -- Сдвиг с поля [-size,+size] на матрицу [1,max]
    ordX = x' + halfX + 1
    ordY = y' + halfY + 1
    -- Int coordinates x y
    coord = (trr ordX, trr ordY)
    trr = floor
-- | проверка выхода за границы поля
controlBounds :: (Double,Double) -> Bool
controlBounds (a,b) = not (cond halfX a || cond halfY b)
  where
    cond size x =
      isNaN x ||
      isInfinite x ||
      x < - size ||
      x >   size
-- | Смешение цветов
-- привнесение в данную точку некоторого цвета
merge
  :: Double       -- ^ цвет в карте градиентов, нормирован по [0,1)
-- А на самом деле выдаёт [0,1]. Баги кружатся и плодятся.
  -> UnsafeColour -- ^ текущее состояние точки (R,G,B,A)
  -- не нормированно, в поле (A)lpha находится счётчик попаданий в точку
  -> UnsafeColour
merge grad
  | grad == 1 = mix $ last $ gradient mainModel
  | otherwise = mix $ (gradient mainModel)!!(floor $ sumGrad*grad)
  where
    -- asking gradient
    -- mixing and increment counter in alpha field
    mix (r,g,b,a) = \(t,h,n,s) -> (r+t,g+h,b+n,s+1)