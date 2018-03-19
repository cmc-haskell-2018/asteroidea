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

