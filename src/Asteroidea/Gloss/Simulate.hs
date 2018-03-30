module Asteroidea.Gloss.Simulate where

import Codec.Picture as Juicy
import Data.Function ((&))
import Graphics.Gloss.Juicy (fromImageRGB8)
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random

import qualified Asteroidea.IFS.Classic as Classic
import Asteroidea.Juicy.Image

-- | Построить изображение для каждой итерации
-- фрактальной системы (кроме первых 20 итераций).
--
-- Для выбора начальных и комбинации новых значений (метаданных)
-- для пикселей используется 'Monoid'.
ifsJuicyPictures
  :: (RandomGen g, Monoid a)
  => (Point -> a)             -- ^ Определить промежуточное значение для каждой точки.
  -> (a -> Juicy.PixelRGB8)   -- ^ Цветовая схема для пикселей.
  -> Classic.IFS              -- ^ Классическая итеративная фрактальная система.
  -> g                        -- ^ Генератор псевдослучайных чисел.
  -> [Picture]
ifsJuicyPictures f render ifs
  = map (fromImageRGB8 . imageImage) . genPictures . map toValue . iterations
  where
    toValue point = (pointCoords point, f point)

    -- убираем первые 20 итераций (которые далеки от решения)
    -- FIXME: нужно вынести число в параметры/константы
    iterations = drop 20 . Classic.chaosGame ifs

    -- строим последовательность изображений,
    -- изменяя на каждом шагу не более одного пикселя
    --
    -- FIXME: используется фиксированный размер экрана.
    genPictures = images render (500, 500)

-- | Координаты пикселя, соответствующего заданной точке.
--
-- FIXME: используется фиксированный размер экрана.
pointCoords :: Point -> (Int, Int)
pointCoords (x, y) = (i, j)
  where
    i = floor (w * (x + 1) / 2)
    j = floor (h * (y + 1) / 2)
    (w, h) = windowSize

-- | Построить изображение для каждой итерации
-- фрактальной системы (кроме первых 20 итераций).
--
-- Для отображения используется чёрно-белая цветовая схема.
--
-- ВНИМАНИЕ: эта реализация предназначена для демонстрации отображения
-- фрактальной системы. Однако она довольно медленная из-за использования 'Picture'.
-- Кроме того, потребление памяти растёт линейно с увеличением количества точек.
--
-- Для эффективного и быстрого отображения фрактальных систем
-- используйте 'ifsJuicyPictures'.
ifsPicturesSimple :: RandomGen g => Classic.IFS -> g -> [Picture]
ifsPicturesSimple ifs g = genPictures iterations
  where
    -- убираем первые 20 итераций (которые далеки от решения)
    -- FIXME: нужно вынести число в параметры/константы
    iterations = drop 20 (Classic.chaosGame ifs g)

    genPictures = foldMapAccum renderPoint

-- | Отобразить точку, используя 'Picture'.
renderPoint :: Point -> Picture
renderPoint (x, y) = polygon [ (0, 0), (0, 1), (1, 1), (1, 0) ]
  & scale (2/w) (2/h)
  & color white
  & translate x y
  & scale (w/2) (h/2)
  where
    (w, h) = windowSize

-- | Анимировать случайную последовательность изображений.
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
    fps = 50000 -- количество итераций в секунду
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

