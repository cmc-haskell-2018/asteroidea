{-|
Module      : GVector
Description : operations with vectors combined with Stdgens (GVec-s)
Copyright   : Just Nothing
Stability   : in progress
-}
module GVector where
import Prelude 
import System.Random 

-- | Вектор Double
-- VS Gloss.Data.Point Float
-- вектор не привязан к СК.
type Vec = (Double, Double)  
-- | Вектор с привязанным к нему генератором
data GVec = GVec {
  gvGen :: StdGen, -- ^ Генератор
  gvVec :: Vec     -- ^ Вектор
} deriving(Show)


-- | Вычленение x,y координат из GVec
gvX :: GVec -> Double
gvX (GVec _ (x ,_)) = x 
gvY :: GVec -> Double
gvY (GVec _ (_ ,y)) = y 

-- | произведение GVec на скаляр
scale ::Double -> GVec -> GVec
scale scl (GVec gen (x,y)) = GVec gen (scl*x , scl*y)

-- | делает GVec'у следующий генератор
nextGen :: GVec -> GVec
nextGen (GVec gen v) = GVec (snd $ next gen) v

-- | Разделение генераторов в два GVec
splitGen :: GVec -> (GVec, GVec)
splitGen (GVec gen0 vec) = ( (GVec gen1 vec),(GVec gen2 vec) )
  where (gen1,gen2) = split gen0

instance Eq GVec where
  (==) gv1 gv2 = gvVec gv1 == gvVec gv2 && (show $ gvGen gv1) == (show $ gvGen gv2)

-- | arctan x/y
phase :: GVec->Double
phase (GVec _ (0,0)) = 0
phase (GVec _ (x,y)) = atan2 y x

-- | arctan y/x
antiPhase :: GVec->Double
antiPhase (GVec _ (0,0)) = 0
antiPhase (GVec _ (x,y)) = atan2 x y

-- | abs value
magnitude :: GVec->Double
magnitude (GVec _ (x,y)) = sqrt (x*x+y*y)

-- | magnitude squared
radiusSqr :: GVec -> Double
radiusSqr (GVec _ (x,y)) = x*x + y*y

instance Num GVec where
  (+) (GVec g (x1,y1)) (GVec _ (x2,y2)) = GVec g ( x1+x2, y1+y2) 
  (*) (GVec g (x1,y1)) (GVec _ (x2,y2)) = GVec g ( x1*x2-y1*y2, x1*y2+x2*y1)
  negate (GVec g (x1,y1)) = GVec g (negate x1, negate y1)
  abs gv@(GVec g _) = GVec g (magnitude gv, 0) 
  signum (GVec g (0,0)) = GVec g (0, 0)
  signum gv@(GVec g (x,y)) = GVec g (x/r, y/r) where r = magnitude gv
  fromInteger i = (GVec (mkStdGen 42) (fromInteger i,0))

instance Fractional GVec where
  fromRational r = (GVec (mkStdGen 42) (fromRational r,0))
  recip gv@(GVec g (x,y)) = GVec g (x/rad,-y/rad)
    where rad = radiusSqr gv

instance Floating GVec where
  pi = (GVec (mkStdGen 42) (pi ,0))
  exp (GVec g (x,y)) = GVec g (expx * cos y , expx * sin y)
    where expx = exp x
  sqrt gv@(GVec g (x,y)) = GVec g (u , (if y < 0 then -v else v))
    where 
      (u,v) = if x < 0 then (v',u') else (u',v')
      v'    = abs y / (u'*2)
      u'    = sqrt ((magnitude gv + abs x) / 2)
  log gv@(GVec g _) = GVec g (magnitude gv, phase gv)
  sin (GVec g (x,y)) = GVec g (sin x * cosh y , cos x * sinh y)
  cos (GVec g (x,y)) = GVec g (cos x * cosh y , (- sin x * sinh y))
  sinh (GVec g (x,y)) = GVec g (cos y * sinh x , sin y * cosh x)
  cosh (GVec g (x,y)) = GVec g (cos y * cosh x , sin y * sinh x)
  asin gv@(GVec g (x,y))  = GVec g (y' , (-x')) 
    where  
      GVec _ (x' , y') = log ( GVec g ((-y) , x) + sqrt (1 - gv*gv))
  acos gv@(GVec g _) = GVec g  (y'' , (-x''))
    where 
      GVec _ (x'' , y'') = log (gv + GVec g ((-y') , x'))
      GVec _ (x' , y')   = sqrt (1 - gv*gv)
  atan gv@(GVec g (x,y))  = GVec g (y',(-x'))
    where 
      GVec _ (x' , y') = log ( (GVec g ((1-y) , x)) / sqrt (1+gv*gv))
  asinh gv = log (gv + sqrt (1+gv*gv))
  acosh gv = log (gv + (sqrt $ gv+1) * (sqrt $ gv-1))
  atanh gv = 0.5 * log ((1.0+gv) / (1.0-gv))