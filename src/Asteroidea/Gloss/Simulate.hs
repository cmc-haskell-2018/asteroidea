module Asteroidea.Gloss.Simulate where

import Data.Function ((&))
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random

import Asteroidea.Coloring
import qualified Asteroidea.IFS.Classic as Classic

-- | Построить изображение для каждой итерации
-- фрактальной системы (кроме первых 20 итераций).
ifsPictures :: RandomGen g => Coloring Point -> Classic.IFS -> g -> [Picture]
ifsPictures coloring ifs g = genPictures iterations
  where
    -- убираем первые 20 итераций (которые далеки от решения)
    -- FIXME: нужно вынести число в параметры/константы
    iterations = drop 20 (Classic.chaosGame ifs g)

    genPictures = foldMapAccum (renderPoint coloring)

-- | Отобразить точку.
renderPoint :: Coloring Point -> Point -> Picture
renderPoint coloring (x, y) = polygon [ (0, 0), (0, 1), (1, 1), (1, 0) ]
  & scale (2/w) (2/h)
  & color (coloringPixels coloring (x, y))
  & translate x y
  & scale (w/2) (h/2)
  where
    (w, h) = windowSize

-- | Анимировать построение итеративной фрактальной системы.
simulateRandomSeries :: (StdGen -> [Picture]) -> IO ()
simulateRandomSeries genPictures = do
  g <- newStdGen
  simulateSeries (genPictures g)

-- | Запустить симуляцию используя зацикленную последовательность изображений.
simulateSeries :: [Picture] -> IO ()
simulateSeries ps = simulate display bgColor fps (cycle ps) render update
  where
    display = InWindow "Asteroidea" windowSize windowOffset
    bgColor = black
    fps = 500 -- количество итераций в секунду
    windowOffset = (100, 100)

    render = mconcat . take 1
    update _ _ = drop 1

-- | Размер окна для симуляции.
windowSize :: Num a => (a, a)
windowSize = (500, 500)

-- | Свернуть набор объектов, сохраняя список промежуточных результатов.
--
-- >>> foldMapAccum show [1..5]
-- ["","1","12","123","1234","12345"]
foldMapAccum :: (Foldable t, Monoid m) => (a -> m) -> t a -> [m]
foldMapAccum f = scanl mappend mempty . foldMap (\x -> [f x])
-- FIXME: эта реализация расставляет скобки как в списке,
-- но мы можем получить более интересное и эффективное поведение,
-- если будем сохранять скобки структуры

