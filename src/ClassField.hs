{-|
Module      : ClassField
Description : operations with field, generating invocation to Model
Copyright   : Just Nothing
Stability   : in progress
-}
module ClassField where

import Graphics.Gloss
import Codec.Picture
import System.Random
import Data.Matrix
import Types
import Const
import GVector()
-- | Обёртка над Field, играющая роль мира. Без грязного IO.
data World =
  World  {
    wField :: Field,  -- ^ your field
    getSGen :: StdGen, -- ^ standart pseudorandom number generator
    busList :: [Vec],  -- ^ BiUnitSquare coverage list
    wModel :: Model
        }
-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)
-- | Бросок с привязанным генератором
type CastGen = (GVec,Double)
-- | Поле есть матрица цветов


initWorld :: StdGen -> World
initWorld sGen = World (createField sizeX sizeY) sGen busPointList mainModel
  where
    sizeX = width mainModel
    sizeY = height mainModel
-- | Создание изначального поля
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
-- | Начальное заполнение фона
initFunction :: Int -> Int -> ((Int,Int)->Cell)
initFunction _ _ =
  (\(a,b) ->
     Cell 0 0 0 0     
  )

-- | TODO Генерация новой точки
-- Дайте мне трансформы, и я сверну мир
newCast :: CastGen -> CastGen
newCast a = a


getNeigbours::Num a =>  a->(a,a)->[(a,a)]
getNeigbours dl (x,y) = [v11,v12,v22,v21]
  where
    v11 = (x+dl,y+dl)
    v12 = (x+dl,y-dl)
    v21 = (x-dl,y+dl)
    v22 = (x-dl,y-dl)
-- | список соседей одного порядка
nthNeigbours :: Int -> [Vec]
nthNeigbours n | n>0 = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2 ** (- fromIntegral n)
nthNeigbours _ = [(0,0)]
-- | Cast Infinite List
-- | бесконечный список соседей
busPointList :: [Vec]
busPointList = concat [ nthNeigbours i | i <- [0,1..]]