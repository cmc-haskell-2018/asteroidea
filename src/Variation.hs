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

defGen :: StdGen
defGen = mkStdGen 42 --For debug purposes

--произведение КОРТЕЖА из генератора и вектора на скаляр
(|*|)::Double->(StdGen,Vec) -> (StdGen,Vec)
(|*|) scl (gen, (x,y)) = (gen, (scl*x , scl*y))

dbgSpherical1 :: Variation
dbgSpherical1 = Var 1 None spherical

dbgSpherical2 :: Variation
dbgSpherical2 = Var (-2) None spherical

dbgAffine :: Variation
dbgAffine = Var 1 (Matrix (AffineMatrix 2 0 0 2 1 1)) affineTransform

calcVariation :: Variation -> (StdGen,Vec)-> (StdGen,Vec)
calcVariation (Var s p f) a = s |*| (f p a)  

radius :: Project
radius (x,y) = sqrt (x*x +y*y)

potent :: Project
potent p = 1 / (radius p) ^ (2::Int)

-- примеры преобразований
spherical :: VariationFunc
spherical _ (gen ,p@(x,y))  = (gen, (coef *x, coef *y))
  where coef = potent p

juliaN :: VariationFunc
juliaN (List (power:dist:_)) (gen,p@(x,y)) = (gen, (r**(dist/power)*(cos t) , r**(dist/power)*(sin t)))
  where r = radius p
        k = fst $ (random gen) :: Double 
        p3 = fromInteger . truncate $ k*power
        t = ((atan2 y x) + 2*pi*p3)/power
juliaN _ a = a

affineTransform :: VariationFunc 
affineTransform (Matrix m) (gen,(x,y)) = (gen, (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m))
affineTransform _ a = a