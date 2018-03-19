{-|
Module      : GVector
Description : operations with vectors combined with Stdgens (GVec-s)
Copyright   : Just Nothing
Stability   : in progress
-}
module GVector where
import Prelude 
import System.Random 
--import Control.Category

import Types

gvX :: GVec -> Double
gvX (GVec _ (x ,_)) = x 

gvY :: GVec -> Double
gvY (GVec _ (_ ,y)) = y 

-- | произведение КОРТЕЖА из генератора и вектора на скаляр
(|*|)::Double -> GVec -> GVec
(|*|) scl (GVec gen (x,y)) = GVec gen (scl*x , scl*y)

nextGen :: GVec -> GVec
nextGen (GVec gen v) = GVec (snd $ next gen) v