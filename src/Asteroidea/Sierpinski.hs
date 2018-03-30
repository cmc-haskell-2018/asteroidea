{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- |
-- Треугольник Серпинского.
module Asteroidea.Sierpinski where

import qualified Asteroidea.IFS.Classic as Classic

-- | Классическая фрактальная система,
-- решением котороя является треугольник Серпинского.
-- 
-- TODO: добавить сюда красивую картинку.
sierpinskiGasket :: Classic.IFS
sierpinskiGasket = map f
  [ \(x, y) -> (x / 2,         y / 2)
  , \(x, y) -> ((x + 1) / 2,   y / 2)
  , \(x, y) -> ((x + 0.5) / 2, (y + sqrt(3)/2) / 2)
  ]
  where
    -- преобразование из би-единичного квадрата в единичный и обратно
    -- (чтобы треугольник был на весь экран)
    f g = toBUS . g . fromBUS

    -- преобразование из би-единичного квадрата в единичный
    fromBUS (x, y) = ((x + 1) / 2, (y + 1) / 2)

    -- преобразование из единичного квадрата в би-единичный
    toBUS   (x, y) = (2 * x - 1, 2 * y - 1)
