{-|
Module      : Plotter
Description : plotting pixels
Copyright   : Just Nothing
Stability   : in progress
-}

module Plotter (initField, updateField, linearFieldIndex) where
import Types
import qualified Gradient                      (colorMap)
import qualified Data.Vector.Unboxed as Vector (unsafeThaw, unsafeFreeze,generate)
import qualified Data.Vector.Unboxed.Mutable as Mutable (modify)
import           Control.Monad.ST              (runST)


-- | Initialize field
initField :: Model -> Field
initField m = Vector.generate (sizeX*sizeY) initFunction
  where
    sizeX = mWidth m
    sizeY = mHeight m
    initFunction = mBackgroundColour m  

-- | Функция размещения в поле ряда точек.
-- Если я успею, то сделаю всё более красиво и понятно.
updateField  ::  Model
  -> [(Vec,Double,Transform)]
  -> Field
  -> Field
updateField model listCast field = let
    listFieldPoints = produceListFromCasts model listCast
  in runST $ do 
     mutableVector <- Vector.unsafeThaw field
     _             <- mapM_
                        (plot mutableVector)
                        listFieldPoints
     updatedField  <- Vector.unsafeFreeze mutableVector
     return updatedField
  where
    plot vector = \(c,v) -> Mutable.modify vector c v

-- | Генерация из списка Cast списка вида (mutate colour, position)
-- для работы 'Vector.Mutable.modify'
produceListFromCasts
  :: Model                -- ^ Параметры преобразований. Зачем я их таскаю?
  -> [(Vec,Double,Transform)]   -- ^ Структура бросков. TODO effective
  -> [(Cell -> Cell,Int)] -- ^ Результат - функция-модификатор и индекс.
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
    -- | проверка, что точка входит в поле (-1,1)
    compose f g = (f model) . (g model) -- TODO right way
    grad = Gradient.colorMap . mGradient

-- | convert index from virtual field as BUS to real field as Vector
pointBUStoFieldPoint
  :: Model            -- параметры модели
  -> (Double, Double) -- точка из би-квадрата
  -> (Int,Int)        -- точка на поле
pointBUStoFieldPoint model (x', y') =
  ( truncate $ scaleX' *( x'+1)
  , truncate $ scaleY' *(-y'+1)
  )
  where 
    scaleX' = half $ mWidth  model
    scaleY' = half $ mHeight model
    half x = (fromIntegral x) /2

-- | TODO alpha blending colours
calcColour :: Transform -> (Double,Double,Double) -> Cell -> Cell
calcColour _ (r1,g1,b1) (r2, g2, b2, a) = ( (r2+r1), (g2+g1), (b2+b1), (a+1))