module Variation where

import Prelude hiding (id, (.))
import Control.Category
import System.Random

data Vector2D = V2 Float Float

instance Num Vector2D where
  V2 x1 y1 + V2 x2 y2 = V2 (x1 + x2) (y1 + y2)

-- | F g a b ~ g -> a -> (g, Maybe b)
--
-- g — генератор псевдослучайных чисел
-- a — тип точек на входе
-- b — тип точек на выходе
newtype F g a b = F
  (g -> a -> (g, Maybe b))

-- | Класс типов Category позволяет использовать
-- (.) и id для F g a b так же, как для функций a -> b.
instance Category (F g) where
  id = F (\g x -> (g, Just x))
  F f . F k = F (\g x -> _)

-- | Для вариаций в 2D мы используем
-- генератор псевдослучайных чисел StdGen.
-- Точками на входе и выходе являются значения типа Vector2D.
type Variation2D = F StdGen Vector2D Vector2D

-- | Класс типов Num позволяет нам определить
-- операции + и * для вариаций, чтобы комбинировать их.
instance Num b => Num (F g a b) where
  F f + F k = F (\g v ->
    f g v `_addMaybes` k g v)

  fromInteger n = F (\g _ -> (g, Just (fromInteger n)))

blur :: Variation2D
blur = _blur

linear :: Variation2D
linear = _linear

combinedExample :: Variation2D
combinedExample = 2 + (blur . linear)

getNeigbours:: Double->Vec->[Vec]
getNeigbours dl (x,y) = [v11,v12,v21,v22]
where
v11 = (x+dl,y+dl)
v12 = (x+dl,y-dl)
v21 = (x-dl,y+dl)
v22 = (x-dl,y-dl)

nthNeigbours :: Integer->[Vec]
nthNeigbours 0 = [(0,0)]
nthNeigbours n = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2**(- (fromInteger n))

biUnitTiling :: [Vec]
biUnitTiling = concat [ nthNeigbours i | i <- [0,1..]]