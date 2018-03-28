module Asteroidea.Simulate.BlackAndWhite where

import Data.Function ((&))
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random

import qualified Asteroidea.IFS.Classic as Classic

simulateIFS :: Classic.IFS -> IO ()
simulateIFS ifs = do
  g <- newStdGen
  simulate display bgColor fps (initialModel g) renderModel updateModel
  where
    display = InWindow "Asteroidea" (500, 500) (200, 200)
    bgColor = black -- цвет фона
    fps = 60 -- количество итераций в секунду

    initialModel = newSimulatedIFS ifs
    renderModel  = sifsPicture
    updateModel _ _ = addNewPoint

data SimulatedIFS = SimulatedIFS
  { sifsPicture    :: Picture  -- ^ Картина с уже сгенерированными точками.
  , sifsIterations :: [Point]  -- ^ Ленивый список новых точек.
  }

newSimulatedIFS :: RandomGen g => Classic.IFS -> g -> SimulatedIFS
newSimulatedIFS ifs g = SimulatedIFS
  { sifsPicture    = blank
    -- FIXME: мы игнорируем первые 20 итераций, нужно это число вынести
  , sifsIterations = drop 20 $ Classic.chaosGame ifs g
  }

addNewPoint :: SimulatedIFS -> SimulatedIFS
addNewPoint sifs = sifs
  { sifsPicture = sifsPicture sifs <> renderPoint p
  , sifsIterations = ps
  }
  where
    (p:ps) = sifsIterations sifs

renderPoint :: Point -> Picture
renderPoint (x, y) = thickCircle (1/w) (2/w)
  & color white
  & translate x y
  & scale (w/2) (h/2)
  where
    w = 500
    h = 500
