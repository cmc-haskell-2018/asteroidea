{-|
Module      : Plotter
Description : plotting pixels
Copyright   : Just Nothing
Stability   : in progress
-}

module Plotter (initField, createField, linearFieldIndex) where
import Types
import qualified Gradient                      (colorMap)
import qualified Data.Vector.Unboxed as Vector (unsafeThaw, unsafeFreeze,generate)
import qualified Data.Vector.Unboxed.Mutable as Mutable (modify)
import           Control.Monad.ST              (runST)


-- | Создание поля, заполнение по mBackgroundColour
initField :: Model -> Field
initField m = Vector.generate (sizeX*sizeY) initFunction
  where
    sizeX = mWidth m
    sizeY = mHeight m
    initFunction = mBackgroundColour m  

-- | Функция создания и заполнения фрактала.
-- И я надеюсь, что когда-нибудь она разделится
-- на создание, размещение одной серии 256,
-- и заполнение всего фрактала
-- в разных функциях, как это было изначально.
createField 
  :: Model                    -- ^ параметры модели
  -> [(Vec,Double,Transform)] -- ^ список точек
  -> Field
createField model listCast = let
    filteredPoints = filter (inBounds model) listCast
    listFieldPoints = produceListFromCasts model filteredPoints
    field = initField model
  in runST $ do 
     mutableVector <- Vector.unsafeThaw field
     _             <- mapM_
                        (plot mutableVector)
                        listFieldPoints
     updatedField  <- Vector.unsafeFreeze mutableVector
     return updatedField
  where
    plot vector = \(c,v) -> Mutable.modify vector c v

-- | Проверка на соответствие границ
-- Лежит ли точка в пределах поля модели?
inBounds :: Model -> (Vec,Double,Transform) -> Bool
inBounds m ((x,y) , _ , _) = flag
  where
    divHW = fromIntegral (mHeight m) / fromIntegral (mWidth m)
    flag = abs x < 1 && abs y < divHW

-- | Генерация из списка Cast списка вида (mutate colour, position)
-- для работы 'Vector.Mutable.modify'
produceListFromCasts
  :: Model                    -- ^ Параметры преобразований.
  -> [(Vec,Double,Transform)] -- ^ Структура бросков.
  -> [(Cell -> Cell,Int)]     -- ^ Результат - функция-модификатор и индекс.
produceListFromCasts model startList =
  map convert startList
  where
    convert (coord,col,ind) = (
        (calcColour ind)
      $ grad model
      $ col        
      ,   (linearFieldIndex model) .
          (pointBUStoFieldPoint model)
      $ coord             )
    grad = Gradient.colorMap . mGradient

-- | Переход от представления точки как вектора в масштабе би-квадрата,
-- к представлению точки как ячейки двумерного поля.
pointBUStoFieldPoint
  :: Model            -- ^ параметры модели
  -> (Double, Double) -- ^ точка из би-квадрата
  -> (Int,Int)        -- ^ точка на поле
pointBUStoFieldPoint m (x, y) =
  ( convert ( x+1)
  , convert (-y+ratio)
  )
  where
    convert = truncate . (width/2 *)
    ratio   = height / width
    width   = fromIntegral (mWidth  m)
    height  = fromIntegral (mHeight m)

-- | Вычисление цвета, в результате размещения точки в поле.
calcColour
  :: Transform              -- ^ трансформа, определяющая поведение функции
  -> (Double,Double,Double) -- ^ цвет для размещения
  -> (Cell -> Cell)         -- ^ функция изменения ячейки поля
calcColour transform (r0,g0,b0) (r2, g2, b2, a) =
  ( (r2+r1), (g2+g1), (b2+b1), (a+opac))
  where
    opac = tOpacity transform
    r1   = r0 * opac
    g1   = g0 * opac
    b1   = b0 * opac