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
dbgAffine1 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0)) affineTransform
-- | DEBUG affine 2
dbgAffine2 :: Variation
dbgAffine2 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0.5)) affineTransform
-- | DEBUG affine 3
dbgAffine3 :: Variation
dbgAffine3 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.5 0)) affineTransform
-- | DEBUG affine 4
dbgAffine4 :: Variation
dbgAffine4 = Var 1 (Matrix (AffineMatrix (-0.5) 0 0 (-0.5) 0.5 0.5)) affineTransform

t1 :: Transform
-- ^ DEBUG transform 1
t1 = templateTransform { tName          = "t1"
               , tVariation     = dbgAffine1
               }

t2 :: Transform
-- ^ DEBUG transform 2
t2 = templateTransform { tName          = "t2"
               , tVariation     = dbgAffine2
               }

t3 :: Transform
-- ^ DEBUG transform 3
t3 = templateTransform { tName          = "t3"
               , tVariation     = dbgAffine3
               }
t4 :: Transform
-- ^ DEBUG transform 4
t4 = templateTransform { tName          = "t4"
               , tVariation     = dbgAffine4
               , tColorPosition = 1
               , tColorSpeed = 0
               }
-- | exampleModel 42
exampleModel :: Model 
exampleModel = templateModel {
                       mTransforms = [t1,t2,t3,t4]
                     , mScale = 80
                     , mRotation = 0
                     , mShiftX = 0.3
                     , mShiftY = 0.3
                     }