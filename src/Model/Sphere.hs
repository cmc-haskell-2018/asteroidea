{-# LANGUAGE NegativeLiterals #-}
{-|
Module      : Sphere
Description : Example fractal: Sphere
Copyright   : Just Nothing
Stability   : Stable
-}
module Model.Sphere (listModel) where
import Variations   (spherical, affine)
import Types
-- | export model
listModel :: [Model]
listModel  = [exampleModel]

-- | DEBUG affine 2
v2 :: AffineMatrix
v2 = AffineMatrix -0.2853 0.35476 -0.35476 -0.285302 -0.36607 0.011145

t1 :: Transform
-- ^ DEBUG transform 1
t1 = templateTransform { 
                tVariation     = spherical 
               }

t2 :: Transform
-- ^ DEBUG transform 2
t2 = templateTransform { 
                 tVariation     = affine v2
               , tColorPosition = 1
               }


-- | exampleModel 42
exampleModel :: Model 
exampleModel = templateModel {
    mName       = "sphere"
  , mTransforms = [t1,t2]
                             }