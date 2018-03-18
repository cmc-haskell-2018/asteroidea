{-|
Module      : Variation
Description : examples and instances for variation, transform, mb Model
Copyright   : Just Nothing
Stability   : in progress
-}
module Variation where
import Prelude  
--import Control.Category
import System.Random
--import Graphics.Gloss
import Types

-- | random generator for debug purposes
defGen :: StdGen
defGen = mkStdGen 42 

-- | произведение КОРТЕЖА из генератора и вектора на скаляр
(|*|)::Double->(StdGen,Vec) -> (StdGen,Vec)
(|*|) scl (gen, (x,y)) = (gen, (scl*x , scl*y))
-- | применение вариации
calcVariation :: Variation -> (StdGen,Vec)-> (StdGen,Vec)
calcVariation (Var s p f) a = s |*| (f p a)

-- | DEBUG sphere 1
dbgSpherical1 :: Variation
dbgSpherical1 = Var 1 None spherical
-- | DEBUG sphere 2
dbgSpherical2 :: Variation
dbgSpherical2 = Var (-2) None spherical
-- | DEBUG affine
dbgAffine :: Variation
dbgAffine = Var 1 (Matrix (AffineMatrix 2 0 0 2 1 1)) affineTransform

-- | отображение в радиус цилиндрических координат
radius :: Project
radius (x,y) = sqrt (x*x +y*y)
-- | отображение в кожффициент потенциала в точке
potent :: Project
potent p = 1 / (radius p) ^ (2::Int)

-- ======== примеры преобразований
-- | сферическое преобразование
spherical :: VariationFunc
spherical _ (gen ,p@(x,y))  = (gen, (coef *x, coef *y))
  where coef = potent p
-- | отображение в стиле множества Жюлиа
juliaN :: VariationFunc
juliaN (List (power:dist:_)) (gen,p@(x,y)) = (gen, (r**(dist/power)*(cos t) , r**(dist/power)*(sin t)))
  where r = radius p
        k = fst $ (random gen) :: Double 
        p3 = fromInteger . truncate $ k*power
        t = ((atan2 y x) + 2*pi*p3)/power
juliaN _ a = a
-- | афинное преобразование
affineTransform :: VariationFunc 
affineTransform (Matrix m) (gen,(x,y)) = (gen, (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m))
affineTransform _ a = a