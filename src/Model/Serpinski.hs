module Model.Serpinski (mainModel) where
import Prelude  
--import Control.Category
import Variations
import Gradient
--import Graphics.Gloss
import Types

mainModel::Model
mainModel = exampleModel

dbgAffine1 :: AffineMatrix
dbgAffine1 = AffineMatrix 0.5 0 0 0.5 0 0
-- | DEBUG affine 2
dbgAffine2 :: AffineMatrix
dbgAffine2 = AffineMatrix 0.5 0 0 0.5 0 0.5
-- | DEBUG affine 3
dbgAffine3 :: AffineMatrix
dbgAffine3 = AffineMatrix 0.5 0 0 0.5 0.5 0
-- | DEBUG affine 4
dbgAffine4 :: AffineMatrix
dbgAffine4 = AffineMatrix (-0.5) 0 0 (-0.5) 0.5 0.5



t1 :: Transform
-- ^ DEBUG transform 1
t1 = templateTransform { 
                tVariation     = affine dbgAffine1
               }

t2 :: Transform
-- ^ DEBUG transform 2
t2 = templateTransform { 
                tVariation     = affine dbgAffine2
               }

t3 :: Transform
-- ^ DEBUG transform 3
t3 = templateTransform { 
                tVariation     = affine dbgAffine3
               }
t4 :: Transform
-- ^ DEBUG transform 4
t4 = templateTransform { 
                tVariation     = affine dbgAffine4 
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