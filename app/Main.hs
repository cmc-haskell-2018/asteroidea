module Main where

import Asteroidea.Coloring
import Asteroidea.Gloss.Simulate
import Asteroidea.Sierpinski

main :: IO ()
main = simulateRandomSeries (ifsPictures blackAndWhite sierpinskiGasket)
