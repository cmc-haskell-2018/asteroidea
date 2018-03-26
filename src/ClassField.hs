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
-- типичное использование - bnw (Brave New World)
data World =
  World {
    -- |  無限画 Main Field, main model parameterized
    -- поле, использован термин Mugen-ga`
    -- для отличия от типа Field и
    -- типичного использования field (экземпляра Field)
    mugenga :: !Field,
    -- | standart pseudorandom number generator
    -- предполагаемое использование ограниченно свойствами getter
    -- что отражено в названии поля
    getSGen :: !StdGen, -- ^ 
    -- | BiUnitSquare coverage infinite list
    -- полное покрытие биквадрата [-1,1]^2
    -- точками (векторами) рекурсивно, см. 'busPointList'
    -- типичное использование одной точки из списка - busPoint
    busList :: [Vec]
        }
-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)
-- | Бросок с привязанным генератором
type CastGen = (GVec,Double)
-- | Поле есть матрица цветов
-- 'UnsafeColour', в отличие от Gloss 'Color',
-- не требует нормирования по [0,1]
-- и оформлен как type
-- но между ними нет прямой совместимости
-- используйте 'mkCol' из 'Asteroidea' для явного преобразования
type Field = Matrix UnsafeColour

-- | Создание изначального поля
-- размером икс на игрек
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
-- | Начальное заполнение фона
-- Фона поля, но не окна.
initFunction
  :: Int -- ^ width
  -> Int -- ^ heigth
  -> ((Int,Int)->UnsafeColour)
  -- ^ function from field point to unsafe colour
initFunction _ _ = \(a,b) ->
    (
      fromIntegral (a `div` 5 + 1) /255,
      fromIntegral (140 - a `div` 54 - b `div` 32) /255,
      fromIntegral (34 + b `div` 5) /255,
      (exp 1)
    )
  -- \_ _ _ -> (0,0,0,1)
{-| ^ веселья ради можно поставить что-то ещё,
 но цвет лесной зелени приятен глазу, как ветви молодых деревьев в летнем саду.
  Заливка градиентом (не по карте градиента, а функцией-генератором):
  (\(a,b) ->
    (
      fromIntegral (a `div` 5 + 1) /255,
      fromIntegral (140 - a `div` 54 - b `div` 32) /255,
      fromIntegral (34 + b `div` 5) /255,
      1.0
    )
  )
  ( \_ -> (0.13,0.54,0.13,1.0))
-}

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
-- пожалуй, первое использование 'seq'
-- (для производительности)
updateWorld
  :: Float -- ^ delta time
  -> World -- ^ Old World
  -> World -- ^ New World
updateWorld dt = \ bnw ->
  bnw `seq` generator bnw (floor $ dt*numCast)

-- | BiUnitSquarePoint  from [-1,1)^2
-- with colour 0.5
busPoint :: World -> CastGen
busPoint bnw = (GVec (getSGen bnw) (head $ busList bnw), 0.5)

-- | генератор нового поля
-- генерируется новая серия бросков одной точки из 'busPoint'
-- в каждой итерации, по счётчику с декрементом
generator
  :: World -- ^ Brave New World
  -> Int   -- ^ counter for loop
  -> World
generator bnw n | n>0 =
  bnw `seq` repack (iter (mugenga bnw, busPoint bnw) 0) (n-1)
  where
    -- repacking and cutting head of BUS list
    repack = \ (f,(gvec,_)) ->
      generator (World f (gvGen gvec) $ tail . busList $ bnw)
generator a _  = a

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
    (choice,newgen) = random $ gvGen gvector
    -- выбор соответствующей ей трансформы
    transform = askTransform choice
  -- применение трансформы к вектору
  in applyTransform transform colour (gvector {gvGen=newgen})

-- | Размещение точки в поле
-- see below 'controlBounds'
-- 'unsafeSet' imported from Data.Matrix
plot
  :: Cast  -- ^ 'Cast' : (point, gradient)
  -> Field -- ^ old field
  -> Field -- ^ new field
{-# INLINE plot #-}
plot ((x, y), colC)
  -- размещение в пределах границ
  | flag = 
     ( \ field -> let
         getPoint (a,b) = unsafeGet a b field -- получение текущего состояния
         colour = merge colC $ getPoint coord -- слияние с точкой на месте
       in unsafeSet colour coord field
     )
  | otherwise = id -- выход за границы
  where
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
-- see below 'merge'
controlBounds :: (Double,Double) -> Bool
{-# INLINE controlBounds #-}
controlBounds (a,b) = not (cond halfX a || cond halfY b)
  where
    cond size x =
      isNaN x ||
      isInfinite x ||
      x < - size ||
      x >   size
-- | Смешение цветов
-- привнесение в данную точку некоторого цвета
-- see below 'mixColour'
merge
  :: Double       -- ^ цвет в карте градиентов, нормирован по [0,1]
  -> UnsafeColour -- ^ текущее состояние точки (R,G,B,A)
  -- не нормированно, в поле (A)lpha находится счётчик попаданий в точку
  -> UnsafeColour
{-# INLINABLE merge #-}
merge grad
  | grad == 1 =
    let
      point = last $ gradient mainModel
    in point `seq` (mixColour point)
  | otherwise =
    let
      point = (gradient mainModel)!!(floor $ sumGrad*grad)
    in point `seq` (mixColour point)
-- | mixing and increment counter in alpha field
-- asking gradient
mixColour :: (Float,Float,Float) -> (UnsafeColour -> UnsafeColour)
{-# INLINE mixColour #-}
mixColour (r,g,b) = \(t,h,n,s) -> (r+t,g+h,b+n,s+1)