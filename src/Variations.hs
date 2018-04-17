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

-- | Variation composition
(|.|) :: Variation->Variation->Variation
(|.|) v1 v2 = Var 1 None compose
  where
    compose _ gv = calcVariation v1 (calcVariation v2 gv)

-- | Variation sum
(|+|) :: Variation->Variation->Variation
(|+|) v1 v2 = Var 1 None sum
  where
    sum _ gv = gv1 + gv2
      where
        gv2@(GVec gen _) = calcVariation v2 gv
        gv' = gv {gvGen = gen} -- новый генератор
        gv1 = calcVariation v1 gv'

-- | отображение в коэффициент потенциала в точке
--potent :: Project
--potent p = 1 / (magnitude p) ^ (2::Int)


-- ======== преобразования

-- | афинное преобразование
affine :: VariationFunc 
affine (Matrix m) g@(GVec _ (x,y)) = g {gvVec = (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m)}
affine _ a = a

-- | сферическое преобразование
spherical :: VariationFunc
spherical _ g@(GVec _ (x,y))  = g{gvVec = (coef *x, coef *y)}
  where coef = 1 / (radiusSqr g)

-- | отображение в стиле множества Жюлиа
juliaN :: VariationFunc --nexGen isn't the most efficient way, you d better take next gen from that (random gen) :: Double from k
juliaN (List (power:dist:_)) g@(GVec gen (x,y)) = nextGen (GVec gen (r**(dist/power)*(cos t) , r**(dist/power)*(sin t))) 
  where r = magnitude g
        k = fst $ (random gen) :: Double 
        p3 = fromInteger . truncate $ k*power
        t = ((atan2 y x) + 2*pi*p3)/power
juliaN _ a = a

-- | линейное преобразование
linear :: VariationFunc
linear _ g = g

-- | синусоидальное преобразование
sinusoidal :: VariationFunc
sinusoidal _ g@(GVec _ (x,y)) = g { gvVec = ((sin x), (sin y))}

-- | swirl
swirl :: VariationFunc
swirl _ g@(GVec _ (x,y)) = g {gvVec = ((x * (sin r2) - y * (cos r2)) , (x * (cos r2) + y * (sin r2)))}
  where r2 = (radiusSqr g)
-- | horseshoe
horseshoe :: VariationFunc
horseshoe _ g@(GVec _ (x,y)) = g {gvVec = (r' * (x - y) * (x + y) , r'*2*x*y)}
  where r' = 1/(magnitude g)

-- | polar
polar :: VariationFunc
polar _ g = g {gvVec = (th/pi, r-1)}
  where th = antiPhase g
        r = magnitude g


-- | disc
disc :: VariationFunc
disc  _ g = g { gvVec = (th'*(sin (pi * r)) ,th' * (cos (pi * r)))}
  where th' = (antiPhase g)/pi
        r = magnitude g

-- | spiral
spiral :: VariationFunc
spiral _ g = g {gvVec = (r' * (cos th + sin r) , r' * (sin th - cos r))}
  where th = antiPhase g
        r = magnitude g
        r' = 1/(magnitude g)

-- | hyperbolic
hyperbolic :: VariationFunc
hyperbolic _ g = g {gvVec = ((sin th)/r, r*(cos th))}
  where th = antiPhase g
        r = magnitude g

-- | square
square :: VariationFunc
square _ (GVec gen _) = GVec n2 (psi1 - 0.5 , psi2 - 0.5)
  where (psi1, n1) = random gen
        (psi2, n2) = random n1

-- | eyefish
eyefish :: VariationFunc
eyefish _ g@(GVec _ (x,y)) = g {gvVec = ( (2/(r+1)) * x , (2/(r+1)) * y)}
  where r = magnitude g

-- | bubble
bubble :: VariationFunc
bubble _ g@(GVec _ (x,y)) = g {gvVec =  ((4/(r2+4)) * x , (4/(r2+4)) * y)}
  where r2 = radiusSqr g

-- | cylinder
cylinder :: VariationFunc
cylinder _ g@(GVec _ (x,y)) = g{gvVec = (sin x , y)}

-- | noise
noise :: VariationFunc
noise _ (GVec gen (x,y)) = GVec n2 (psi1 * x * (cos (2*pi*psi2)) , psi1 * y * (sin (2*pi*psi2)))
  where (psi1, n1) = random gen
        (psi2, n2) = random n1 

-- | blur
blur :: VariationFunc
blur _ (GVec gen _) = GVec n2 (psi1 * (cos (2*pi*psi2)) , psi1 * (sin (2*pi*psi2)))
  where (psi1, n1) = random gen
        (psi2, n2) = random n1

-- | gaussian
gaussian :: VariationFunc
gaussian _ gv = GVec n5 (s . cos $ arg , s . sin $ arg)
  where
    n0 = gvGen gv
    (psi1, n1) = random n0
    (psi2, n2) = random n1
    (psi3, n3) = random n2
    (psi4, n4) = random n3
    (psi5, n5) = random n4
    s = (*) $ psi1 + psi2 + psi3 + psi4 - 2
    arg = 2*pi*psi5

-- | exponential 
exponential :: VariationFunc
exponential (List (dx:dy:_)) g@(GVec _ (x,y)) = g{gvVec = ((exp (x - 1 + dx)) * (cos (pi*(y+dy))) , (exp (x - 1 + dx)) * (sin (pi*(y+dy))))}
exponential _ a = a

-- | покомпонентный квадрат
-- квадрат, разреженный с краёв. смыкается по осям к (0,0)
eachSquare :: VariationFunc
eachSquare _ g@(GVec _ (x,y)) = g {gvVec =  (x * x , y * y)}

-- | гипербола
-- Действительно, гипербола. Даже две. Но где комплементарная?
hyperb :: VariationFunc
hyperb _ g@(GVec _ (x,y)) = g {gvVec =  (x / y , y / x)}

-- | сумма, домноженная на оси
-- д.б. вдоль осей сильно вытянутой
-- Фактически, вытянутая парабола, сходящаяся к (0,0)
sumMultAxis :: VariationFunc
sumMultAxis _ g@(GVec _ (x,y)) = g {gvVec = ((x+y) * x , (x+y) * y )}