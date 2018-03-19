{-|
Module      : Variations
Description : basically a module with Variation Functions
Copyright   : Just Nothing
Stability   : in progress
-}
module Variations where
import Prelude  
--import Control.Category
import System.Random
import Types

-- | применение вариации
calcVariation :: Variation -> GVec-> GVec
calcVariation (Var s p f) a = s |*| (f p a)

-- | проекция модуль радиус-вектора полярных координат
radius :: Project
radius (x,y) = sqrt (x*x +y*y)
-- | отображение в коэффициент потенциала в точке
potent :: Project
potent p = 1 / (radius p) ^ (2::Int)


-- ======== преобразования
-- | сферическое преобразование
spherical :: VariationFunc
spherical _ (GVec gen p@(x,y))  = GVec gen (coef *x, coef *y)
  where coef = potent p

-- | отображение в стиле множества Жюлиа
juliaN :: VariationFunc --nexGen isn't the most efficient way, you d better take next gen from that (random gen) :: Double from k
juliaN (List (power:dist:_)) (GVec gen p@(x,y)) = nextGen (GVec gen (r**(dist/power)*(cos t) , r**(dist/power)*(sin t))) 
  where r = radius p
        k = fst $ (random gen) :: Double 
        p3 = fromInteger . truncate $ k*power
        t = ((atan2 y x) + 2*pi*p3)/power
juliaN _ a = a

-- | афинное преобразование
affineTransform :: VariationFunc 
affineTransform (Matrix m) (GVec gen (x,y)) = GVec gen (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m)
affineTransform _ a = a


