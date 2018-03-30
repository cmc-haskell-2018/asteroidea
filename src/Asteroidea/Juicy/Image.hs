{-# LANGUAGE FlexibleContexts #-}
-- |
-- Реализация быстрой отрисовки изображений
-- для итераций фрактальных систем.
module Asteroidea.Juicy.Image where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Monad.ST (runST)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector.Mutable

import Asteroidea.Juicy.Coloring (blackPixel8)

-- | Изображение вместе с данными, по которым отрисован каждый пиксель.
--
-- Например, в качестве данных может выступать плотность пикселя
-- (сколько точек приходится на каждый пиксель).
--
-- ВНИМАНИЕ: в каждый момент времени должно использоваться только одно изображение.
-- Изображения должны использоваться по порядку.
data Image pixel a = Image
  { imageWidth  :: Int                -- ^ Ширина изображения.
  , imageHeight :: Int                -- ^ Высота изображения.
  , imageImage  :: Juicy.Image pixel  -- ^ Изображение (отрисованные пиксели).
  , imageValues :: Vector a           -- ^ Матрица с данными.
  }

-- | Построить серию изображений,
-- обрабатывая входные данные по одному значению,
-- и обновляя на каждом шагу не более одного пикселя.
--
-- В отличие от 'imageIterations' для начальных значений
-- и комбинации промежуточных значений используется 'Monoid'.
--
-- Начальный цвет фона — чёрный (см. 'blackPixel8').
images
  :: (Juicy.Pixel pixel, Juicy.ColorConvertible Juicy.Pixel8 pixel, Monoid a)
  => (a -> pixel)       -- ^ Как отрисовывать пиксели.
  -> (Int, Int)         -- ^ Размер изображений (ширина и высота).
  -> [((Int, Int), a)]  -- ^ Последовательность входных данных с координатами.
  -> [Image pixel a]
images render (w, h)
  = imageIterations mappend render (backgroundImage w h black mempty)
  where
    black = Juicy.promotePixel blackPixel8

-- | Построить серию изображений,
-- обрабатывая входные данные по одному значению,
-- и обновляя на каждом шагу не более одного пикселя.
--
-- ВНИМАНИЕ: в каждый момент времени должно использоваться только одно изображение.
-- Изображения должны использоваться по порядку.
imageIterations
  :: Juicy.Pixel pixel
  => (a -> a -> a)      -- ^ Как обновлять данные.
  -> (a -> pixel)       -- ^ Как отрисовывать пиксели.
  -> Image pixel a      -- ^ Начальное изображение.
  -> [((Int, Int), a)]  -- ^ Последовательность входных данных с координатами.
  -> [Image pixel a]
imageIterations f render = scanl addPoint
  where
    addPoint image ((i, j), x) = unsafeUpdatePixelAt (i, j) (f x) render image

-- | Фоновое изображение со значениями по умолчанию.
backgroundImage
  :: Juicy.Pixel pixel
  => Int            -- ^ Ширина.
  -> Int            -- ^ Высота.
  -> pixel          -- ^ Фоновый пиксель.
  -> a              -- ^ Значение по умолчанию.
  -> Image pixel a
backgroundImage w h pixel def = Image
  { imageWidth  = w
  , imageHeight = h
  , imageImage  = Juicy.generateImage (\_ _ -> pixel) w h
  , imageValues = Vector.replicate (w * h) def
  }

-- | Обновить один пиксель.
--
-- ВНИМАНИЕ: предыдущая версия изображения не должна использоваться!
unsafeUpdatePixelAt
  :: Juicy.Pixel pixel
  => (Int, Int)                               -- ^ Координаты пикселя.
  -> (a -> a)                                 -- ^ Функция преобразования данных.
  -> (a -> pixel)    -- ^ Функция отрисовки пикселя.
  -> Image pixel a                            -- ^ Исходное изображение.
  -> Image pixel a
unsafeUpdatePixelAt (i, j) _ _ image
  -- если пиксель за границами изображения — игнорируем его
  | i < 0 || i >= imageWidth image
    || j < 0 || j >= imageHeight image
    = image
unsafeUpdatePixelAt (i, j) f render image = runST $ do
  mutableVector <- Vector.unsafeThaw (imageValues image)
  x <- Vector.Mutable.read mutableVector k
  Vector.Mutable.write mutableVector k (f x)
  newValues <- Vector.unsafeFreeze mutableVector
  return image
    { imageImage = setJuicyPixelAt (i, j) (render x) (imageImage image)
    , imageValues = newValues
    }
  where
    k = linearPixelIndex image (i, j)

-- | Линейный индекс пикселя (для доступа к данным).
linearPixelIndex :: Image pixel a -> (Int, Int) -> Int
linearPixelIndex image (i, j) = i + j * imageWidth image

-- | Выставить один пиксель в изображении @'Juicy.Image' pixel@.
setJuicyPixelAt
  :: Juicy.Pixel pixel
  => (Int, Int)         -- ^ Координаты пикселя.
  -> pixel              -- ^ Значение пикселя.
  -> Juicy.Image pixel  -- ^ Исходное изображение.
  -> Juicy.Image pixel
setJuicyPixelAt (i, j) pixel image = runST $ do
  mutableImage <- Juicy.unsafeThawImage image
  let k = Juicy.mutablePixelBaseIndex mutableImage i j
  Juicy.unsafeWritePixel (Juicy.mutableImageData mutableImage) k pixel
  Juicy.unsafeFreezeImage mutableImage
