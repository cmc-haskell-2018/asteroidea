module Model.Tile (mainModel) where
import Prelude  
--import Control.Category
import Variations
import Gradient
--import Graphics.Gloss
import Types

mainModel :: Model
mainModel = exampleModel


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
grad = paletteToDouble "4B6ECA0AA1F007B8EC78DAF7A3E9EBB3E9E9A4EBE5A4E9E66AAEEB04B8EB03B9EB1EB6CB6BB7AB7FCBA5B0BC94CFAA52B87F28CD493ABD0646EB1922FA0D17E90600A50700890D0B49180728211B203026222F263313383C11466A457B8D6E74D48195EC93AFD19DA1869CA9637C663E3935501F23661F0B78130B890B0C970C079F060096090292100099220E9B3328B87944E38885E8A0A4FFBDC4E6DDD6E6E4E7EDE9EAEBEBEBEAEBEDEBEAF0EEF2FEE5F2FFE9EAEEDDC2D5C0AED4A998CC9362B1BE748DED6D64E5663DDE393DAA313AAA2D319B0217BC032CC60153B201978E59C3908CC79FA7CECCC0C4BED6DAA7C6D88EADC15CA7AD3087A20A8C8E3181A62E6AB35151BF5E74DA8D9DC1ADBDBDB6D89BD4D496DAD0C7CCE2CB98DCB793D78E599A621974312270323949598153539C2A298621174B20192B13201F112200104A00194A032B69063C62084270005A750372790A7A6C2A766C5960723A4199061D9D1F1ABC0636C82563C84468BC629B927499AB847EBE9FA8B9B6B9C2CBB2AEC7B9B9A3BAC28DA4AA30899B0A5D3F024232202F283A27433E495B66517C8C60C1A193C8D2CCCCDAE5D7D7D5D6C9BBBBB295B48A6DC19A61C29A68CBAC94D0C4E8DCE3E6EBE1EBEAB5EDE4A1E2E474AFC154A3CA12A2A241979468ADAA6AAAB487D2CD89EAE182ECEC05B9EC00BAEB09A3ED0088D90080C203548C37235E8C192A9412129217109125227A45417F6C7081A1A090C2E3B1E5E3DAE9ECE4E6E5D3E5D7E4C6BBEBE3BCEBE8E3E4E8DAB9E1D697DEB491C7694B915D0E626C0A43342030262131242B30191A1C06201A04330D024319015620007E12129C0F08AC030AD8050BED0105EA0200D10900B93A05A7803FA5BA91D7D9CCE6EDE6E5EBEBE5E9ECE7E8EAEBEAE6E2D1CAD1B8B4DABBA9CFAC6CC188379F7135706E355449132431270A2D63073E762B31AB305BC40078CE0290CE28798C5D646C684C247814166A100761010D681714812F236B76666B787E6A87994A3EBE3E20A20A326D053A30003B2A0137290434280731251F2F250A272301232500121D160F05"