{-|
Module      : Variation
Description : examples and instances for variation, transform, mb Model
Copyright   : Just Nothing
Stability   : in progress
-}
module Variation where
import Prelude  
--import Control.Category
import System.Random
import Graphics.Gloss
import Types
import GVector

-- | применение вариации
calcVariation :: Variation -> GVec-> GVec
calcVariation (Var s p f) a = s |*| (f p a)

radius :: Project
radius (x,y) = sqrt (x*x +y*y)
-- | отображение в кожффициент потенциала в точке
potent :: Project
potent p = 1 / (radius p) ^ (2::Int)

-- ======== примеры преобразований
-- | сферическое преобразование
spherical :: VariationFunc
spherical _ (GVec gen p@(x,y))  = GVec gen (coef *x, coef *y)
  where coef = potent p
-- | отображение в стиле множества Жюлиа
juliaN :: VariationFunc --nexGen isn't the most efficient way, you d better take next gen from that (random gen) :: Double from k
juliaN (List (power:dist:_)) (GVec gen p@(x,y)) = nextGen (GVec gen (r**(dist/power)*(cos t) , r**(dist/power)*(sin t))) 
  where r = radius p
        k = fst $ (random gen) :: Double 
        p3 = fromInteger . truncate $ k*power
        t = ((atan2 y x) + 2*pi*p3)/power
juliaN _ a = a
-- | афинное преобразование
affineTransform :: VariationFunc 
affineTransform (Matrix m) (GVec gen (x,y)) = GVec gen (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m)
affineTransform _ a = a

{--
   <palette count="256" format="RGB">
      4B6ECA0AA1F007B8EC78DAF7A3E9EBB3E9E9A4EBE5A4E9E6
      6AAEEB04B8EB03B9EB1EB6CB6BB7AB7FCBA5B0BC94CFAA52
      B87F28CD493ABD0646EB1922FA0D17E90600A50700890D0B
      49180728211B203026222F263313383C11466A457B8D6E74
      D48195EC93AFD19DA1869CA9637C663E3935501F23661F0B
      78130B890B0C970C079F060096090292100099220E9B3328
      B87944E38885E8A0A4FFBDC4E6DDD6E6E4E7EDE9EAEBEBEB
      EAEBEDEBEAF0EEF2FEE5F2FFE9EAEEDDC2D5C0AED4A998CC
      9362B1BE748DED6D64E5663DDE393DAA313AAA2D319B0217
      BC032CC60153B201978E59C3908CC79FA7CECCC0C4BED6DA
      A7C6D88EADC15CA7AD3087A20A8C8E3181A62E6AB35151BF
      5E74DA8D9DC1ADBDBDB6D89BD4D496DAD0C7CCE2CB98DCB7
      93D78E599A621974312270323949598153539C2A29862117
      4B20192B13201F112200104A00194A032B69063C62084270
      005A750372790A7A6C2A766C5960723A4199061D9D1F1ABC
      0636C82563C84468BC629B927499AB847EBE9FA8B9B6B9C2
      CBB2AEC7B9B9A3BAC28DA4AA30899B0A5D3F024232202F28
      3A27433E495B66517C8C60C1A193C8D2CCCCDAE5D7D7D5D6
      C9BBBBB295B48A6DC19A61C29A68CBAC94D0C4E8DCE3E6EB
      E1EBEAB5EDE4A1E2E474AFC154A3CA12A2A241979468ADAA
      6AAAB487D2CD89EAE182ECEC05B9EC00BAEB09A3ED0088D9
      0080C203548C37235E8C192A9412129217109125227A4541
      7F6C7081A1A090C2E3B1E5E3DAE9ECE4E6E5D3E5D7E4C6BB
      EBE3BCEBE8E3E4E8DAB9E1D697DEB491C7694B915D0E626C
      0A43342030262131242B30191A1C06201A04330D02431901
      5620007E12129C0F08AC030AD8050BED0105EA0200D10900
      B93A05A7803FA5BA91D7D9CCE6EDE6E5EBEBE5E9ECE7E8EA
      EBEAE6E2D1CAD1B8B4DABBA9CFAC6CC188379F7135706E35
      5449132431270A2D63073E762B31AB305BC40078CE0290CE
      28798C5D646C684C247814166A100761010D681714812F23
      6B76666B787E6A87994A3EBE3E20A20A326D053A30003B2A
      0137290434280731251F2F250A272301232500121D160F05
   </palette>
--}
