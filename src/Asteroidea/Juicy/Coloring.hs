-- |
-- Цветовые схемы для генерации изображений при помощи JuicyPixels.
module Asteroidea.Juicy.Coloring where

import Data.Monoid
import qualified Codec.Picture.Types as Juicy

-- * Цветовые схемы

-- | Чёрно-белая цветовая схема.
blackAndWhite :: Any -> Juicy.PixelRGB8
blackAndWhite (Any True) = Juicy.promotePixel whitePixel8
blackAndWhite _ = Juicy.promotePixel blackPixel8

-- | Линейная цветовая схема по плотности (оттенки серого).
linearDensity8 :: Sum Int -> Juicy.PixelRGB8
linearDensity8 (Sum n) = Juicy.PixelRGB8 x x x
  where
    x = fromIntegral n

-- * Вспомогательные определения

-- | Чёрный пиксель.
blackPixel8 :: Juicy.Pixel8
blackPixel8 = 0

-- | Белый пиксель.
whitePixel8 :: Juicy.Pixel8
whitePixel8 = 255

