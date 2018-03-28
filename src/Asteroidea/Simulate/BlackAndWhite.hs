module Asteroidea.Simulate.BlackAndWhite where

import Data.Function ((&))
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random

import qualified Asteroidea.IFS.Classic as Classic

-- | Запустить симуляции фрактальной системы.
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

-- | Модель симуляции фрактальной системы.
data SimulatedIFS = SimulatedIFS
  { sifsPicture    :: Picture  -- ^ Картина с уже сгенерированными точками.
  , sifsIterations :: [Point]  -- ^ Ленивый список новых точек.
  }

-- | Создать модель для симуляции фрактальной системы.
newSimulatedIFS :: RandomGen g => Classic.IFS -> g -> SimulatedIFS
newSimulatedIFS ifs g = SimulatedIFS
  { -- начинаем с чистого листа
    sifsPicture    = blank
    -- FIXME: мы игнорируем первые 20 итераций, нужно это число вынести
  , sifsIterations = drop 20 $ Classic.chaosGame ifs g
  }

-- | Расчитать очередную итерацию фрактальной системы
-- и добавить соответствующую точку.
addNewPoint :: SimulatedIFS -> SimulatedIFS
addNewPoint sifs = sifs
  { sifsPicture = sifsPicture sifs <> renderPoint p
  , sifsIterations = ps
  }
  where
    (p:ps) = sifsIterations sifs

-- | Отобразить точку.
renderPoint :: Point -> Picture
renderPoint (x, y) = thickCircle (1/w) (2/w)
  & color white
  & translate x y
  & scale (w/2) (h/2)
  where
    w = 500
    h = 500
