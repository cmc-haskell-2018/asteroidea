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
import qualified Control.Monad       as M      (fmap, mfilter,MonadPlus)
import           Control.Monad.ST              (runST)
import qualified Data.Foldable       as F      (mapM_)


-- | Initialize field
initField :: Model -> Field
initField m = Vector.generate (sizeX*sizeY) initFunction
  where
    sizeX = mWidth m
    sizeY = mHeight m
    initFunction = mBackgroundColour m  

-- | Функция размещения в поле ряда точек.
-- Если я успею, то сделаю всё более красиво и понятно.
updateField  :: 
   ( M.MonadPlus  t
   , Foldable     t
   )
  => Model
  -> t (Vec,Double,Int)
  -> Field
  -> Field
updateField model listCast field = let
    listFieldPoints = produceListFromCasts model listCast
  in runST $ do 
     mutableVector <- Vector.unsafeThaw field
                   -- TODO Effective implementation
     _             <- F.mapM_ (
                      ( (\f (c,v) -> f c v) )
                      $ (Mutable.modify mutableVector)
                           )
                      $ listFieldPoints
     updatedField  <- Vector.unsafeFreeze mutableVector
     return updatedField

-- | Генерация из списка Cast списка вида (mutate colour, position)
-- для работы 'Vector.Mutable.modify'
produceListFromCasts
  :: M.MonadPlus t
  => Model                -- ^ Параметры преобразований. Зачем я их таскаю?
  -> t (Vec,Double,Int)   -- ^ Структура бросков. TODO effective
  -> t (Cell -> Cell,Int) -- ^ Результат - функция-модификатор и индекс.
produceListFromCasts model startList =
  -- require Functor
  M.fmap convert filteredList
  where
    convert (coord,col,ind) = (
        compose
          (calcColour ind)
          grad
      $ col        
      , compose
          linearFieldIndex
          pointBUStoFieldPoint
      $ coord             )
    filteredList =
        -- require MonadPlus
        M.mfilter inBounds
        startList
    -- | проверка, что точка входит в поле (-1,1)
    compose f g = (f model) . (g model) -- TODO right way
    grad = Gradient.colorMap . mGradient
    inBounds :: ((Double,Double),b,c) -> Bool
    inBounds ((x,y),_,_) = (control x) && (control y)
    control x = (x > - 1) && (x < 1)

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
calcColour :: Int -> Model -> (Double,Double,Double) -> Cell -> Cell
calcColour _ _ (r1,g1,b1) (r2, g2, b2, a) = ( (r2+r1), (g2+g1), (b2+b1), (a+1))