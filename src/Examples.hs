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
import Gradient
--import Graphics.Gloss
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
-- | DEBUG affine 5
dbgAffine5 :: Variation
dbgAffine5= Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0)) affineTransform
-- | DEBUG affine 6
dbgAffine6 :: Variation
dbgAffine6 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.5 0)) affineTransform
-- | DEBUG affine 7
dbgAffine7 :: Variation
dbgAffine7 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.25 0.5)) affineTransform
-- | DEBUG square
dbgSquare :: Variation
dbgSquare = Var (1) None square
-- | DEBUG GVec
dbgGVec :: GVec
dbgGVec = GVec defGen (1,1)

t1 :: Transform
-- ^ DEBUG transform 1
t1 = Transform { tName          = "t1"
               , tVariation     = dbgAffine1
               , tWeight        = 1
               , tColorPosition = 1
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t2 :: Transform
-- ^ DEBUG transform 2
t2 = Transform { tName          = "t2"
               , tVariation     = dbgAffine2
               , tWeight        = 1
               , tColorPosition = 0.889
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t3 :: Transform
-- ^ DEBUG transform 3
t3 = Transform { tName          = "t3"
               , tVariation     = dbgAffine3
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }
t4 :: Transform
-- ^ DEBUG transform 4
t4 = Transform { tName          = "t4"
               , tVariation     = dbgAffine4
               , tWeight        = 1
               , tColorPosition = 1
               , tColorSpeed    = 1
               , tOpacity       = 1
               , tXaos          = []
               }
-- | exampleModel 42
exampleModel :: Model 
exampleModel = Model { mName = "42"
                     , mTransforms = [t1,t2,t3,t4]
                     , mCamera = Nothing
                     , mGradient = grad
                     , mWidth = 1024
                     , mHeight = 1024
                     , mScale = 50
                     , mRotation = 0
                     }
  where
grad = paletteToDouble currentGradient