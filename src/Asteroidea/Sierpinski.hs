module Asteroidea.Sierpinski where

import qualified Asteroidea.IFS.Classic as Classic

-- | Классическая фрактальная система,
-- решением котороя является треугольник Серпинского.
-- 
-- TODO: добавить сюда красивую картинку.
sierpinskiGasket :: Classic.IFS
sierpinskiGasket =
  [ \(x, y) -> (x / 2,       y / 2)
  , \(x, y) -> ((x + 1) / 2, y / 2)
  , \(x, y) -> (x / 2, (y + 1) / 2)
  ]
