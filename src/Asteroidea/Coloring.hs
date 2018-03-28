module Asteroidea.Coloring where

import Graphics.Gloss.Data.Color

-- | Схема раскраски итераций фрактальной системы.
data Coloring pixel = Coloring
  { coloringBackground :: Color
    -- ^ Цвет фона.
  , coloringPixels     :: pixel -> Color
    -- ^ Как раскрашивать пиксели с ненулевой плотностью?
  }

-- | Чёрно-белая схема расцветки (без оттенков серого).
blackAndWhite :: Coloring pixel
blackAndWhite = Coloring
  { coloringBackground = black
  , coloringPixels = const white
  }
