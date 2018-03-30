-- |
-- Реализация классических итеративных фрактальных систем.
module Asteroidea.IFS.Classic where

import System.Random

-- | Точка в двумерном пространстве.
type Point = (Float, Float)

-- | Афинное преобразование следующего вида:
--
-- F(x, y) = (ax + by + c, dx + ey + f)
--
-- FIXME: для ускорения генерации можно использовать матрицы.
type AffineTransform = Point -> Point

-- | Классическая фрактальная система содержит несколько афинных преобразований.
type IFS = [AffineTransform]

-- | Детерминированный вариант 'chaosGame'.
--
-- На вход подаются заранее сгенерированные случайные значения.
-- На выходе — бесконечный список точек.
orderGame
  :: Point              -- ^ Начальная точка (выбранная случайно).
  -> [AffineTransform]  -- ^ Список преобразований (сгенерированный случайно).
  -> [Point]            -- ^ Последовательность точек.
orderGame = scanl (flip ($))

-- | Игра «хаос» для решения классической фрактальной системы.
--
-- >>> take 3 $ chaosGame [id] (mkStdGen 5)
-- [(0.88574517,-0.3738985),(0.88574517,-0.3738985),(0.88574517,-0.3738985)]
--
-- >>> take 3 $ chaosGame [\(x, y) -> (x+1/2, y)] (mkStdGen 5)
-- [(0.88574517,-0.3738985),(1.3857452,-0.3738985),(1.8857452,-0.3738985)]
chaosGame :: RandomGen g => IFS -> g -> [Point]
chaosGame fs g = orderGame randomPoint randomTransforms
  where
    (g', g'') = split g
    randomTransforms = randomElements fs g'
    (randomPoint, _) = randomBiUnitSquarePoint g''

-- | Случайная точка из би-единичного квадрата:
--
-- @
-- -1 <= x <= 1
-- -1 <= y <= 1
-- @
--
-- >>> fst $ randomBiUnitSquarePoint (mkStdGen 5)
-- (0.77490556,-0.3762896)
randomBiUnitSquarePoint :: RandomGen g => g -> (Point, g)
randomBiUnitSquarePoint g = ((x, y), g'')
  where
    (x, g')  = randomR (-1, 1) g
    (y, g'') = randomR (-1, 1) g'

-- | Бесконечный список из случайно выбранных значений.
--
-- >>> take 30 $ randomElements ['a'..'z'] (mkStdGen 1)
-- "xuvjyjsxielpchcdguqkulkyjjxsvs"
--
-- >>> take 30 $ randomElements [1..10] (mkStdGen 1) :: [Int]
-- [6,7,6,8,9,8,5,6,1,9,2,6,5,2,7,4,3,1,7,5,5,8,1,5,8,2,6,5,2,7]
randomElements :: RandomGen g => [a] -> g -> [a]
randomElements xs g = map choose randomIndices
  where
    choose i = xs !! i  -- FIXME: можно использовать Vector, чтобы ускорить
    randomIndices = randomRs (0, length xs - 1) g
