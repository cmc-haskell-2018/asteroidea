{-# LANGUAGE NegativeLiterals #-}
{-|
Module      : Serpinski
Description : Example fractal: Sierpinski triangle
Copyright   : Just Nothing
Stability   : Stable
-}
module Model.Serpinski (mainModel) where
import Variations (affine, exponential, mirrorX, mirrorY)
import Types
-- | export model
mainModel::Model
mainModel = exampleModel

-- | DEBUG affine 1
dbgAffine1 :: AffineMatrix
dbgAffine1 = stdMatrix
-- | DEBUG affine 2
dbgAffine2 :: AffineMatrix
dbgAffine2 = stdMatrix { oy = 0.5}
-- | DEBUG affine 3
dbgAffine3 :: AffineMatrix
dbgAffine3 = stdMatrix { ox = 0.5}
-- | DEBUG affine 4
dbgAffine4 :: AffineMatrix
dbgAffine4 = stdMatrix { xx = -0.5, yy = -0.5, ox = 0.5, oy = 0.5}

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
                     , mRotation = 90
                     , mFinal = Just templateTransform { tVariation =  ( exponential 1 0) . (affine $ AffineMatrix 2.5 (-2.5) 1 1 (-2.15) 0 ) . mirrorX . mirrorY  }               
                     }