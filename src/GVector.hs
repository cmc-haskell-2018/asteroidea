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
  vgGen :: StdGen,
  vgVec :: Vec
}

gvX :: GVec -> Double
gvX (GVec _ (x ,_)) = x 

gvY :: GVec -> Double
gvY (GVec _ (_ ,y)) = y 

-- | произведение GVec на скаляр
(|*|)::Double -> GVec -> GVec
(|*|) scl (GVec gen (x,y)) = GVec gen (scl*x , scl*y)

nextGen :: GVec -> GVec
nextGen (GVec gen v) = GVec (snd $ next gen) v

instance Eq GVec where
  (==) gv1 gv2 = vgVec gv1 == vgVec gv2

phase :: GVec->Double
phase (GVec _ (0,0)) = 0
phase (GVec _ (x,y)) = atan2 y x

antiPhase :: GVec->Double
antiPhase (GVec _ (0,0)) = 0
antiPhase (GVec _ (x,y)) = atan2 x y

magnitude :: GVec->Double
magnitude (GVec _ (x,y)) = sqrt (x*x +y*y)

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

