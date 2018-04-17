module Model.Sphere (mainModel) where  
--import Control.Category
import Variations
--import Graphics.Gloss
import Types

mainModel :: Model
mainModel = exampleModel



v2 :: AffineMatrix
v2 = AffineMatrix (-0.2853) (0.35476) (-0.35476) (-0.285302) (-0.36607) (0.011145)

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
                       mTransforms = [t1,t2]
                     }