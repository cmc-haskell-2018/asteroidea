module Model.Hex (mainModel) where
import Prelude  
--import Control.Category
import Variations
import Gradient
--import Graphics.Gloss
import Types

mainModel :: Model
mainModel = exampleModel


af1 :: Variation
af1 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 1 0)) affineTransform

af2 :: Variation
af2 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.49975 0.866169)) affineTransform

af3 :: Variation
af3 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0.49975 (-0.86616))) affineTransform

af4 :: Variation
af4 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 (-0.49975) (-0.86616))) affineTransform

af5 :: Variation
af5 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 (-0.49975) 0.86616)) affineTransform

af6 :: Variation
af6 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 (-1) 0)) affineTransform

af7 :: Variation
af7 = Var 1 (Matrix (AffineMatrix 0.5 0 0 0.5 0 0)) affineTransform



t1 :: Transform
t1 = templateTransform { tName          = "t1"
               , tVariation     = af1
               , tColorPosition = 1
               , tColorSpeed    = 0
               }

t2 :: Transform
t2 = templateTransform { tName          = "t1"
               , tVariation     = af2
               }
t3 :: Transform
t3 = templateTransform { tName          = "t1"
               , tVariation     = af3
               }

t4 :: Transform
t4 = templateTransform { tName          = "t1"
               , tVariation     = af4
               }

t5 :: Transform
t5 = templateTransform { tName          = "t1"
               , tVariation     = af5
               }

t6 :: Transform
t6 = templateTransform { tName          = "t1"
               , tVariation     = af6
               , tColorPosition = 1
               , tColorSpeed    = 0
               }

t7 :: Transform
t7 = templateTransform { tName          = "t1"
               , tVariation     = af7
               , tColorPosition = 0
               , tColorSpeed    = 0.8
               }

-- | exampleModel 42
exampleModel :: Model 
exampleModel = exampleModel {
                       mTransforms = [t1,t2,t3,t4,t5,t6,t7]
                     , mGradient = grad
                     }
  where
grad = paletteToDouble "\
\00003A00004000003D00002500001C0000250D00433D00947037DFB8A0FFCABE\
\FFCABEFFCDCAFFE2DFFFD0D3FF9A7FFF8573FF9A91FFDFE2FFFFFFFFFFFFFFF1\
\CAFF8834FA6A19EB8E4CFFF1E5FFFFFFFFFFFDFFA661FF6410E522008E2B008E\
\583DCA9497FF8888FF3D1CC400004600001C0100491000611C00793700944C00\
\AF9161FFEEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF\
\FFFFFFD9FFDFA6FF5E22BB37008200004900004300005200006D00007013007F\
\4300AC8B6DFFF4EBFFFFFFFFFFFFFFFFFFFFE8D0FFD6CDFFE8DCFFF1EBFFBEA3\
\FFA682FFB288FFEBBEFFFFC4FFB897FF5837FF0000BB00006D00002E00002E00\
\002500003A00004000003D00002500001C0000250D00433D00947037DFB8A0FF\
\CABEFFCABEFFCDCAFFE2DFFFD0D3FF9A7FFF8573FF9A91FFDFE2FFFFFFFFFFFF\
\FFF1CAFF8834FA6A19EB8E4CFFF1E5FFFFFFFFFFFDFFA661FF6410E522008E2B\
\008E583DCA9497FF8888FF3D1CC400004600001C0100491000611C0079370094\
\4C00AF9161FFEEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF\
\FFFFFFFFFFD9FFDFA6FF5E22BB37008200004900004300005200006D00007013\
\007F4300AC8B6DFFF4EBFFFFFFFFFFFFFFFFFFFFE8D0FFD6CDFFE8DCFFF1EBFF\
\BEA3FFA682FFB288FFEBBEFFFFC4FFB897FF5837FF0000BB00006D00002E0000\
\2500003A00004000003D00002500001C0000250D00433D00947037DFB8A0FFCA\
\BEFFCABEFFCDCAFFE2DFFFD0D3FF9A7FFF8573FF9A91FFDFE2FFFFFFFFFFFFFF\
\F1CAFF8834FA6A19EB8E4CFFF1E5FFFFFFFFFFFDFFA661FF6410E522008E2B00\
\8E583DCA9497FF8888FF3D1CC400004600001C0100491000611C00793700944C\
\00AF9161FFEEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF\
\FFFFFFFFD9FFDFA6FF5E22BB37008200004900004300005200006D0000701300\
\7F4300AC8B6DFFF4EBFFFFFFFFFFFFFFFFFFFFE8D0FFD6CDFFE8DCFFF1EBFFBE\
\A3FFA682FFB288FFEBBEFFFFC4FFB897FF5837FF0000BB00006D00002E000025"