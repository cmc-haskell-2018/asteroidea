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
t1 = Transform { tName          = "t1"
               , tVariation     = dbgAffine1
               , tWeight        = 10
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t2 :: Transform
-- ^ DEBUG transform 2
t2 = Transform { tName          = "t2"
               , tVariation     = dbgAffine2
               , tWeight        = 1
               , tColorPosition = 0
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
               , tColorSpeed    = 0
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
                     , mBackgroundColour = (\_->(0,0,0,0))
                     , mOuterIter = 21845
                     , mInnerIter = 200
                     }
  where
grad = paletteToDouble "AA3B1CAA3F1C8144254454254D6D2551792F51712F5D713865854B758D5589A15E8DA254C2AD41DA8B2FDE7725D6672FD66A25C25F1C914C254C6925345012204409203C122030122038122C48124069256189388DB25EAAC67AEED17AFAE154E6972EE67A25E67A25DB6925CA5E25B64C1C853712542B09241B091C20001C3009204009405C1C54793869914B91AE6796B27195AE718DAA71718D55516D4240442F24341C1018120C0C090408000004000004000004000000000400000C0000100B00240F00441712812612AA3712C25F1CB27C2F9AAE67A6C27AAACA7AB2D28DBEDAA0BED297CAD697E2DE84B6DA8DB6D28DBACE8DBECE97C6CEA9D2DAB3EAE6C5EAEAC5FAF1BCF6E5BCEAC08DD2AC96BECA96B2C697B6CE8DB6CA8DB6C684AACA84A6C6849EBA7A8DB27171954B5175383C542530401C1838091428091418090C1009070C090408090804091407091C0F09241709502712812F12B23B12C65725DB6625DE6625DE751CE2871CE68E1CF29225FA9B1CF6BB1CEA7F2FFABC67FAE170FAE154FED32EFECF2EFED04ADAB55E957867597942405D2F203C12142C09041800000C000408000004000004000004000400000400000404000404000408000408000408000004000000000000090000000000000000000000000000000000000400000404000C0800101000201000381B095933097D4309A63F12B24812C65A1CBE6B259D703899632F59793855653871481CB65025CE6225D26625D66E1CD66E1CDA7625DE6E1CE27225DE6E2FEE6F41C6A054A99470A1BA7AAAC284BECE97CED6A0E2EEBCF6EEC5FEFAFEFAFECEF2FECEE6EEC5C6D6A0AAC284919571546D423840251C2C12182409141409201309300F09652712892F12A63313913712652F124427122038091C2C09141C09101409081000081000040C00041000081409102009143012284812446D2561813889AA6795B67191B67181A15E69854B4459383C4C253040253C441C713712A63313BE4C1CCA5B1CD25E1CD2621CC657129E37096922002C0F001404000C00000804000408000810001020091C3409"