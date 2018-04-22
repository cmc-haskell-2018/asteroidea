{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Variations
Description : basically a module with Variation Functions
Copyright   : Just Nothing
Stability   : in progress
-}
module Variations where
import Prelude 
import Data.Function
import RND
import Types

-- | convert binary GVec operation to binary Variation operation
binGVecToVar :: (GVec->GVec->GVec)->Variation->Variation->Variation
binGVecToVar op = flip . (flip (\v -> on op ($v)))

type PlainVariation = (Vec -> Vec)

applyGVec :: PlainVariation -> Variation
{-# INLINABLE affine #-}
applyGVec func = \g -> g {gvVec = func $ gvVec g}

{-
instance Eq Variation where
  (==) v1 v2 = and [t1,t2,t3,t4]
   where
    g = mempty
    t1 = (v1 $ GVec g (1,0)) == (v2 $ GVec g (1,0))
    t2 = (v1 $ GVec g (-1,-1)) == (v2 $ GVec g (-1,-1))
    t3 = (v1 $ GVec g (0.05,-0.234)) == (v2 $ GVec g (0.05,-0.234))
    t4 = (v1 $ GVec g (-1123,1.1)) == (v2 $ GVec g (-1123,1.1))
-}


instance Num Variation where
  (+) = binGVecToVar (+)
  (*) = binGVecToVar (*)
  abs v = abs . v
  negate v = negate . v
  signum v = signum . v
  fromInteger i = (\ (GVec g _) -> GVec g (fromInteger i,0))

instance Fractional Variation where
  fromRational r = (\ (GVec g _) -> GVec g (fromRational r,0))
  (/) = binGVecToVar (/)

-- ======== преобразования
-- | афинное преобразование
{- @
 [ xx xy ox ]   [x]    [xx*x + xy*y + ox]
 [ yx yy oy ] x [y] == [yx*x + yy*y + oy]
 [ 0  0  1  ]   [1]    [        1       ]
   @
-}
affine :: AffineMatrix -> Variation
{-# INLINE[~1] affine #-}
affine (AffineMatrix sx 0 0 sy 0 0)
  g@(GVec _ (x,y))
  = g {gvVec = (x * sx, y * sy)}
affine (AffineMatrix sx 0 0 sy a b)
  g@(GVec _ (x,y))
  = g {gvVec = (x * sx + a, y * sy + b)}
affine (AffineMatrix xx0 xy0 yx0 yy0 a b)
  g@(GVec _ (x,y))
  = g {gvVec = (x', y')}
  where
    x' = xx0 * x + xy0 * y + a
    y' = yx0 * x + yy0 * y + b

-- | сферическое преобразование
spherical :: Variation
spherical  g@(GVec _ (x,y))  = g{gvVec = (coef *x, coef *y)}
  where coef = 1 / (radiusSqr g)

-- | отображение в стиле множества Жюлиа
juliaN ::Double-> Double-> Variation 
juliaN  power dist g@(GVec gen _) =
  GVec gen' (r**(dist/power)*(cos t) , r**(dist/power)*(sin t))
  where r = magnitude g
        (k, gen') = randomR (0,1) gen
        p3 = fromInteger . truncate $ k*power
        t = (phase g + 2*pi*p3)/power

-- | линейное преобразование
linear :: Variation
linear g = g

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
  where (psi1, n1) = randomR (0,1) gen
        (psi2, n2) = randomR (0,1) n1

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
  where (psi1, n1) = randomR (0,1) gen
        (psi2, n2) = randomR (0,1) n1 

-- | blur
blur :: Variation
blur (GVec gen _) = GVec n2 (psi1 * (cos (2*pi*psi2)) , psi1 * (sin (2*pi*psi2)))
  where (psi1, n1) = randomR (0,1) gen
        (psi2, n2) = randomR (0,1) n1

-- | gaussian
gaussian :: Variation
gaussian gv = GVec n5 (s . cos $ arg , s . sin $ arg)
  where
    n0 = gvGen gv
    (psi1, n1) = randomR (0,1) n0
    (psi2, n2) = randomR (0,1) n1
    (psi3, n3) = randomR (0,1) n2
    (psi4, n4) = randomR (0,1) n3
    (psi5, n5) = randomR (0,1) n4
    s = (*) $ psi1 + psi2 + psi3 + psi4 - 2
    arg = 2*pi*psi5

-- | exponential 
exponential :: Double -> Double -> Variation
exponential dx dy g@(GVec _ (x,y)) =
  g { gvVec
           =( expX * cos phi
            , expX * sin phi
            )
    }
  where
    expX = (exp $ x - 1 + dx)
    phi  = pi*(y+dy)

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

-- | случайное отражение от оси абсцисс
mirrorX :: Variation
mirrorX (GVec g (x,y)) = GVec g' (x',y)
  where
    (i,g') = randomR (False,True) g
    x' = if i then x else negate x

-- | случайное отражение от оси ординат
mirrorY :: Variation
mirrorY (GVec g (x,y)) = GVec g' (x,y')
  where
    (i,g') = randomR (False,True) g
    y' = if i then y else negate y

-- | ?
mirrorR :: Variation
mirrorR gv@(GVec g (x,y)) = GVec g' (x',y')
  where
    r = radiusSqr gv
    (i,g') = randomR (False,True) g
    ff = if i then id else (/r)
    y' = ff y
    x' = ff x