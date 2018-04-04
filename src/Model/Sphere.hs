module Model.Sphere (mainModel) where
import Prelude  
--import Control.Category
import Variations
import Gradient
--import Graphics.Gloss
import Types

mainModel :: Model
mainModel = exampleModel


v1 :: Variation
v1 = Var 1 None spherical
-- | DEBUG affine 2
v2 :: Variation
v2 = Var 1 (Matrix (AffineMatrix (-0.2853) (0.35476) (-0.35476) (-0.285302) (-0.36607) (0.011145))) affineTransform

t1 :: Transform
-- ^ DEBUG transform 1
t1 = templateTransform { tName          = "t1"
               , tVariation     = v1
               }

t2 :: Transform
-- ^ DEBUG transform 2
t2 = templateTransform { tName          = "t2"
               , tVariation     = v2
               , tColorPosition = 1
               }


-- | exampleModel 42
exampleModel :: Model 
exampleModel = templateModel {
                       mTransforms = [t1,t2]
                     }