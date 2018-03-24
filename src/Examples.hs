{-|
Module      : Examples
Description : for debug purposes
Copyright   : Just Nothing
Stability   : in progress
-}
module Examples where
import Prelude  
--import Control.Category
import Variations
import System.Random
import Graphics.Gloss
import Types

-- | random generator for debug purposes
defGen :: StdGen
defGen = mkStdGen 47
-- | DEBUG sphere 1
dbgSpherical1 :: Variation
dbgSpherical1 = Var 1 None spherical
-- | DEBUG sphere 2
dbgSpherical2 :: Variation
dbgSpherical2 = Var (-2) None spherical
-- | DEBUG affine 1
dbgAffine1 :: Variation
dbgAffine1 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.5 0.5)) affineTransform
-- | DEBUG affine 2
dbgAffine2 :: Variation
dbgAffine2 = Var 1 (Matrix (AffineMatrix 0 0.5 (-0.5) 0 (-0.5) (-0.5))) affineTransform
-- | DEBUG affine 3
dbgAffine3 :: Variation
dbgAffine3 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.5 (-0.5))) affineTransform
-- | DEBUG affine 4
dbgAffine4 :: Variation
dbgAffine4 = Var 1 (Matrix (AffineMatrix 0 0.5 (-0.5) 0 (-0.5) 0.5)) affineTransform
-- | DEBUG square
dbgSquare :: Variation
dbgSquare = Var (1) None square
-- | DEBUG GVec
dbgGVec :: GVec
dbgGVec = GVec defGen (1,1)

ds :: Transform
ds = Transform "DS" dbgSquare 1 1 0.1 1 []
t1 :: Transform
-- ^ DEBUG transform 1
t1 = Transform "t1" dbgAffine1 1 1 0 1 []
t2 :: Transform
-- ^ DEBUG transform 2
t2 = Transform "t2" dbgAffine2 1 0.889 0 1 []
t3 :: Transform
-- ^ DEBUG transform 3
t3 = Transform "t3" dbgAffine3 1 1 0 1 []
t4 :: Transform
-- ^ DEBUG transform 4
t4 = Transform "t4" dbgAffine4 1 1 1 1 []
-- | exampleModel 42
exampleModel :: Model 
exampleModel = Model "42" [ds] Nothing grad 512 512 4 0
  where
   grad = stdGrad 128

dbgAffine5 :: Variation
dbgAffine5= Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0)) affineTransform

dbgAffine6 :: Variation
dbgAffine6 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.5 0)) affineTransform

dbgAffine7 :: Variation
dbgAffine7 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0.5)) affineTransform

t5 :: Transform
-- ^ DEBUG transform 1
t5 = Transform "t1" dbgAffine5 1 0 0.25 1 []
t6 :: Transform
-- ^ DEBUG transform 2
t6 = Transform "t2" dbgAffine6 1 0.5 0.25 1 []
t7 :: Transform
-- ^ DEBUG transform 3
t7 = Transform "t3" dbgAffine7 1 1 0.25 1 []
exampleModel2 :: Model 
exampleModel2 = Model "Seprinsky" [t6,t5,t7] Nothing grad 512 512 250 0
  where
   grad = stdGrad 128

