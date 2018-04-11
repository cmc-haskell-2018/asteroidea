module Model.Square (mainModel) where
import Prelude  
--import Control.Category
import Variations
import Gradient
--import Graphics.Gloss
import Types

mainModel :: Model
mainModel = exampleModel


v1 :: Variation
v1 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 0 0)) affineTransform

v2 :: Variation
v2 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 0.333333333 0)) affineTransform

v3 :: Variation
v3 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 0 (-0.333333333))) affineTransform

v4 :: Variation
v4 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 (-0.333333333) 0)) affineTransform

v5 :: Variation
v5 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 0 0.333333333)) affineTransform

v6 :: Variation
v6 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 0.333333333 0.333333333)) affineTransform

v7 :: Variation
v7 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 (-0.333333333) 0.333333333)) affineTransform

v8 :: Variation
v8 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 (-0.333333333) (-0.333333333))) affineTransform

v9 :: Variation
v9 = Var 1 (Matrix (AffineMatrix 0.333333333 0 0 0.333333333 0.333333333 (-0.333333333))) affineTransform




t1 :: Transform
t1 = templateTransform { tName          = "t1"
               , tVariation     = v1
               , tColorPosition = 1
               , tColorSpeed    = 0
               }


t2 :: Transform
t2 = templateTransform { tName          = "t1"
               , tVariation     = v2
               , tColorPosition = 0
               , tColorSpeed    = 0
               }

t3 :: Transform
t3 = templateTransform { tName          = "t1"
               , tVariation     = v3
               }

t4 :: Transform
t4 = templateTransform { tName          = "t1"
               , tVariation     = v4
               }

t5 :: Transform
t5 = templateTransform { tName          = "t1"
               , tVariation     = v5
               }

t6 :: Transform
t6 = templateTransform { tName          = "t1"
               , tVariation     = v6
               }

t7 :: Transform
t7 = templateTransform { tName          = "t1"
               , tVariation     = v7
               }

t8 :: Transform
t8 = templateTransform { tName          = "t1"
               , tVariation     = v8
               }

t9 :: Transform
t9 = templateTransform { tName          = "t1"
               , tVariation     = v9
               }

-- | exampleModel 42
exampleModel :: Model 
exampleModel = templateModel { 
                       mTransforms = [t1,t2,t3,t4,t5,t6,t7,t8,t9]
                     , mGradient = grad
                     , mFinal = Just templateTransform { tVariation = Var 4 (List [-1,0]) exponential }
                     }
  where
grad = paletteToDouble "\
\4B6ECA0AA1F007B8EC78DAF7A3E9EBB3E9E9A4EBE5A4E9E66AAEEB04B8EB03B9\
\EB1EB6CB6BB7AB7FCBA5B0BC94CFAA52B87F28CD493ABD0646EB1922FA0D17E9\
\0600A50700890D0B49180728211B203026222F263313383C11466A457B8D6E74\
\D48195EC93AFD19DA1869CA9637C663E3935501F23661F0B78130B890B0C970C\
\079F060096090292100099220E9B3328B87944E38885E8A0A4FFBDC4E6DDD6E6\
\E4E7EDE9EAEBEBEBEAEBEDEBEAF0EEF2FEE5F2FFE9EAEEDDC2D5C0AED4A998CC\
\9362B1BE748DED6D64E5663DDE393DAA313AAA2D319B0217BC032CC60153B201\
\978E59C3908CC79FA7CECCC0C4BED6DAA7C6D88EADC15CA7AD3087A20A8C8E31\
\81A62E6AB35151BF5E74DA8D9DC1ADBDBDB6D89BD4D496DAD0C7CCE2CB98DCB7\
\93D78E599A621974312270323949598153539C2A298621174B20192B13201F11\
\2200104A00194A032B69063C62084270005A750372790A7A6C2A766C5960723A\
\4199061D9D1F1ABC0636C82563C84468BC629B927499AB847EBE9FA8B9B6B9C2\
\CBB2AEC7B9B9A3BAC28DA4AA30899B0A5D3F024232202F283A27433E495B6651\
\7C8C60C1A193C8D2CCCCDAE5D7D7D5D6C9BBBBB295B48A6DC19A61C29A68CBAC\
\94D0C4E8DCE3E6EBE1EBEAB5EDE4A1E2E474AFC154A3CA12A2A241979468ADAA\
\6AAAB487D2CD89EAE182ECEC05B9EC00BAEB09A3ED0088D90080C203548C3723\
\5E8C192A9412129217109125227A45417F6C7081A1A090C2E3B1E5E3DAE9ECE4\
\E6E5D3E5D7E4C6BBEBE3BCEBE8E3E4E8DAB9E1D697DEB491C7694B915D0E626C\
\0A43342030262131242B30191A1C06201A04330D024319015620007E12129C0F\
\08AC030AD8050BED0105EA0200D10900B93A05A7803FA5BA91D7D9CCE6EDE6E5\
\EBEBE5E9ECE7E8EAEBEAE6E2D1CAD1B8B4DABBA9CFAC6CC188379F7135706E35\
\5449132431270A2D63073E762B31AB305BC40078CE0290CE28798C5D646C684C\
\247814166A100761010D681714812F236B76666B787E6A87994A3EBE3E20A20A\
\326D053A30003B2A0137290434280731251F2F250A272301232500121D160F05"