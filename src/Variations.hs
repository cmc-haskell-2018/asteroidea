{-|
Module      : Variations
Description : basically a module with Variation Functions
Copyright   : Just Nothing
Stability   : in progress
-}
{-# LANGUAGE FlexibleInstances #-}
module Variations where
import Prelude  
--import Control.Category
import System.Random
import Types

-- | произведение GVec на скаляр
(|*|)::Double -> Variation -> Variation
(|*|) scl v =  (scale scl) . v 

instance Eq Variation where
  (==) v1 v2 = True 

{-
-- | Variation sum
(|+|) :: Variation->Variation->Variation
(|+|) v1 v2 = Var 1 None sum
  where
    sum _ gv = gv1 + gv2
      where
        gv2@(GVec gen _) = calcVariation v2 gv
        gv' = gv {gvGen = gen} -- новый генератор
        gv1 = calcVariation v1 gv'
-}
-- | отображение в коэффициент потенциала в точке
--potent :: Project
--potent p = 1 / (magnitude p) ^ (2::Int)


-- ======== преобразования

-- | афинное преобразование
affine :: AffineMatrix -> Variation 
affine m g@(GVec _ (x,y)) = g {gvVec = (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m)}
affine _ a = a

-- | сферическое преобразование
spherical :: Variation
spherical  g@(GVec _ (x,y))  = g{gvVec = (coef *x, coef *y)}
  where coef = 1 / (radiusSqr g)

-- | отображение в стиле множества Жюлиа
juliaN ::Double-> Double-> Variation 
juliaN  power dist g@(GVec gen (x,y)) = GVec gen' (r**(dist/power)*(cos t) , r**(dist/power)*(sin t))
  where r = magnitude g
        (k, gen') = random gen
        p3 = fromInteger . truncate $ k*power
        t = ((atan2 y x) + 2*pi*p3)/power

-- | линейное преобразование
linear :: Variation
linear  g = g

-- | синусоидальное преобразование
sinusoidal :: Variation
sinusoidal g@(GVec _ (x,y)) = g { gvVec = ((sin x), (sin y))}

-- | swirl
swirl :: Variation
swirl  g@(GVec _ (x,y)) = g {gvVec = ((x * (sin r2) - y * (cos r2)) , (x * (cos r2) + y * (sin r2)))}
  where r2 = (radiusSqr g)
-- | horseshoe
horseshoe :: Variation
horseshoe  g@(GVec _ (x,y)) = g {gvVec = (r' * (x - y) * (x + y) , r'*2*x*y)}
  where r' = 1/(magnitude g)

-- | polar
polar :: Variation
polar g = g {gvVec = (th/pi, r-1)}
  where th = antiPhase g
        r = magnitude g


-- | disc
disc :: Variation
disc  g = g { gvVec = (th'*(sin (pi * r)) ,th' * (cos (pi * r)))}
  where th' = (antiPhase g)/pi
        r = magnitude g

-- | spiral
spiral :: Variation
spiral g = g {gvVec = (r' * (cos th + sin r) , r' * (sin th - cos r))}
  where th = antiPhase g
        r = magnitude g
        r' = 1/(magnitude g)

-- | hyperbolic
hyperbolic :: Variation
hyperbolic g = g {gvVec = ((sin th)/r, r*(cos th))}
  where th = antiPhase g
        r = magnitude g

-- | square
square :: Variation
square (GVec gen _) = GVec n2 (psi1 - 0.5 , psi2 - 0.5)
  where (psi1, n1) = random gen
        (psi2, n2) = random n1

-- | eyefish
eyefish :: Variation
eyefish g@(GVec _ (x,y)) = g {gvVec = ( (2/(r+1)) * x , (2/(r+1)) * y)}
  where r = magnitude g

-- | bubble
bubble :: Variation
bubble g@(GVec _ (x,y)) = g {gvVec =  ((4/(r2+4)) * x , (4/(r2+4)) * y)}
  where r2 = radiusSqr g

-- | cylinder
cylinder :: Variation
cylinder g@(GVec _ (x,y)) = g{gvVec = (sin x , y)}

-- | noise
noise :: Variation
noise (GVec gen (x,y)) = GVec n2 (psi1 * x * (cos (2*pi*psi2)) , psi1 * y * (sin (2*pi*psi2)))
  where (psi1, n1) = random gen
        (psi2, n2) = random n1 

-- | blur
blur :: Variation
blur (GVec gen _) = GVec n2 (psi1 * (cos (2*pi*psi2)) , psi1 * (sin (2*pi*psi2)))
  where (psi1, n1) = random gen
        (psi2, n2) = random n1

-- | gaussian
gaussian :: Variation
gaussian gv = GVec n5 (s . cos $ arg , s . sin $ arg)
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
exponential :: Double-> Double-> Variation
exponential dx dy g@(GVec _ (x,y)) = g{gvVec = ((exp (x - 1 + dx)) * (cos (pi*(y+dy))) , (exp (x - 1 + dx)) * (sin (pi*(y+dy))))}

-- | покомпонентный квадрат
-- квадрат, разреженный с краёв. смыкается по осям к (0,0)
eachSquare :: Variation
eachSquare g@(GVec _ (x,y)) = g {gvVec =  (x * x , y * y)}

-- | гипербола
-- Действительно, гипербола. Даже две. Но где комплементарная?
hyperb :: Variation
hyperb g@(GVec _ (x,y)) = g {gvVec =  (x / y , y / x)}

-- | сумма, домноженная на оси
-- д.б. вдоль осей сильно вытянутой
-- Фактически, вытянутая парабола, сходящаяся к (0,0)
sumMultAxis :: Variation
sumMultAxis g@(GVec _ (x,y)) = g {gvVec = ((x+y) * x , (x+y) * y )}