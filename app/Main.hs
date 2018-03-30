module Main where

import Asteroidea.Gloss.Simulate
import Asteroidea.Juicy.Coloring
import Asteroidea.Sierpinski

main :: IO ()
main = simulateRandomSeries (ifsJuicyPictures pointValue render sierpinskiGasket)
  where
    -- каждая точка добавляет 1 к плотности
    pointValue _ = 1

    -- для отрисовки используем линейную расцветку по плотности
    -- плотность умножаем на 15, чтобы быстрее увидеть результат
    render density = linearDensity8 (15 * density)
