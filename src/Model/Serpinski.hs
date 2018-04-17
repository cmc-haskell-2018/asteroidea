module Model.Serpinski (mainModel) where
import Prelude  
--import Control.Category
import Variations
import Gradient
--import Graphics.Gloss
import Types

mainModel::Model
mainModel = exampleModel

dbgAffine1 :: Variation
dbgAffine1 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0)) affine
-- | DEBUG affine 2
dbgAffine2 :: Variation
dbgAffine2 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0.5)) affine
-- | DEBUG affine 3
dbgAffine3 :: Variation
dbgAffine3 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.5 0)) affine
-- | DEBUG affine 4
dbgAffine4 :: Variation
dbgAffine4 = Var 1 (Matrix (AffineMatrix (-0.5) 0 0 (-0.5) 0.5 0.5)) affine

sphere :: Variation
sphere = Var 0.25 None spherical

blur1 :: Variation
blur1 = Var 0.004 None blur

t1 :: Transform
-- ^ DEBUG transform 1
t1 = templateTransform { 
                tVariation     = dbgAffine1
               }

t2 :: Transform
-- ^ DEBUG transform 2
t2 = templateTransform { 
                tVariation     = dbgAffine2
               }

t3 :: Transform
-- ^ DEBUG transform 3
t3 = templateTransform { 
                tVariation     = dbgAffine3
               }
t4 :: Transform
-- ^ DEBUG transform 4
t4 = templateTransform { 
                tVariation     = (blur1 |+| sphere) |.| dbgAffine4 
               , tColorPosition = 1
               , tColorSpeed = 0
               }
-- | exampleModel 42
exampleModel :: Model 
exampleModel = templateModel {
                       mTransforms = [t1,t2,t3,t4]
                     , mScale = 1
                     , mRotation = 0
                     , mShiftX = -0.3
                     , mShiftY = -0.3
                     }