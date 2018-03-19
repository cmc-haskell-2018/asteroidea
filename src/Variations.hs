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

-- | отображение в коэффициент потенциала в точке
--potent :: Project
--potent p = 1 / (magnitude p) ^ (2::Int)


-- ======== преобразования
-- | сферическое преобразование
spherical :: VariationFunc
spherical _ g@(GVec gen (x,y))  = GVec gen (coef *x, coef *y)
  where coef = 1 / (radiusSqr g)

-- | отображение в стиле множества Жюлиа
juliaN :: VariationFunc --nexGen isn't the most efficient way, you d better take next gen from that (random gen) :: Double from k
juliaN (List (power:dist:_)) g@(GVec gen (x,y)) = nextGen (GVec gen (r**(dist/power)*(cos t) , r**(dist/power)*(sin t))) 
  where r = magnitude g
        k = fst $ (random gen) :: Double 
        p3 = fromInteger . truncate $ k*power
        t = ((atan2 y x) + 2*pi*p3)/power
juliaN _ a = a

-- | афинное преобразование
affineTransform :: VariationFunc 
affineTransform (Matrix m) (GVec gen (x,y)) = GVec gen (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m)
affineTransform _ a = a

-- | линейное преобразование
linear :: VariationFunc
linear _ vec = vec

-- | синусоидальное преобразование
sinusoidal :: VariationFunc
sinusoidal _ (GVec gen (x,y)) = GVec gen ((sin x), (sin y))

-- | swirl
swirl :: VariationFunc
swirl _ g@(GVec gen (x,y)) = GVec gen ((x * (sin r2) - y * (cos r2)) , (x * (cos r2) + y * (sin r2)))
  where r2 = (radiusSqr g)
-- | horseshoe
horseshoe :: VariationFunc
horseshoe _ g@(GVec gen (x,y)) = GVec gen (r' * (x - y) * (x + y) , r'*2*x*y)
  where r' = 1/(magnitude g)

-- | polar
polar :: VariationFunc
polar _ g@(GVec gen _) = GVec gen (th/pi, r - 1)
  where th = antiPhase g
        r = magnitude g

-- | disc
disc :: VariationFunc
disc  _ g@(GVec gen _) = GVec gen (th'*(sin (pi * r)) ,th' * (cos (pi * r)))
  where th' = (antiPhase g)/pi
        r = magnitude g

-- | spiral
spiral :: VariationFunc
spiral _ g@(GVec gen _) = GVec gen (r' * (cos th + sin r) , r' * (sin th - cos r))
  where th = antiPhase g
        r = magnitude g
        r' = 1/(magnitude g)

-- | hyperbolic
hyperbolic :: VariationFunc
hyperbolic _ g@(GVec gen _) = GVec gen ( (sin th)/r, r*(cos th) )
  where th = antiPhase g
        r = magnitude g

-- | square
square :: VariationFunc
square _ (GVec gen _) = GVec (snd n2) (psi1 - 0.5 , psi2 - 0.5)
  where n1 = random gen 
        psi1 = fst n1 
        n2 = random (snd n1) 
        psi2 = fst n2 

