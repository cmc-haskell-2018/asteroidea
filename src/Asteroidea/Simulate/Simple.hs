module Asteroidea.Simulate.Simple where

import Data.Function ((&))
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random

import qualified Asteroidea.IFS.Classic as Classic
import Asteroidea.Coloring

-- | Запустить симуляции фрактальной системы.
simulateIFS :: Coloring Point -> Classic.IFS -> IO ()
simulateIFS coloring ifs = do
  g <- newStdGen
  simulate display bgColor fps (initialModel g) renderModel updateModel
  where
    display = InWindow "Asteroidea" (500, 500) (200, 200)
    bgColor = coloringBackground coloring -- цвет фона
    fps = 60 -- количество итераций в секунду

    initialModel = newSimulatedIFS ifs
    renderModel  = sifsPicture
    updateModel _ _ = addNewPoint coloring

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
addNewPoint :: Coloring Point -> SimulatedIFS -> SimulatedIFS
addNewPoint coloring sifs = sifs
  { sifsPicture = sifsPicture sifs <> renderPoint coloring p
  , sifsIterations = ps
  }
  where
    (p:ps) = sifsIterations sifs

-- | Отобразить точку.
renderPoint :: Coloring Point -> Point -> Picture
renderPoint coloring (x, y) = thickCircle (1/w) (2/w)
  & color (coloringPixels coloring (x, y))
  & translate x y
  & scale (w/2) (h/2)
  where
    w = 500
    h = 500
