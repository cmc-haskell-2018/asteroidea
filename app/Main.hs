module Main where

import Asteroidea.Coloring
import Asteroidea.Simulate.Simple
import Asteroidea.Sierpinski

main :: IO ()
main = simulateIFS blackAndWhite sierpinskiGasket
