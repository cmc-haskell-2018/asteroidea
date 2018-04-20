module Model.Hex (mainModel) where

--import Control.Category
import Variations
import Gradient
--import Graphics.Gloss
import Types

mainModel :: Model
mainModel = exampleModel


af1 :: AffineMatrix
af1 = AffineMatrix 0.5 0 0 0.5 1 0

af2 :: AffineMatrix
af2 = AffineMatrix 0.5 0 0 0.5 0.49975 0.866169

af3 :: AffineMatrix
af3 = AffineMatrix 0.5 0 0 0.5 0.49975 (-0.86616)

af4 :: AffineMatrix
af4 = AffineMatrix 0.5 0 0 0.5 (-0.49975) (-0.86616)

af5 :: AffineMatrix
af5 = AffineMatrix 0.5 0 0 0.5 (-0.49975) 0.86616

af6 :: AffineMatrix
af6 = AffineMatrix 0.5 0 0 0.5 (-1) 0

af7 :: AffineMatrix
af7 = AffineMatrix 0.5 0 0 0.5 0 0



t1 :: Transform
t1 = templateTransform { 
                tVariation     = affine af1
               , tColorPosition = 1
               , tColorSpeed    = 0
               , tXaos = [1,0,0,1,1,1,1]
               }

t2 :: Transform
t2 = templateTransform { 
                tVariation     = affine af2
               , tXaos = [0,1,1,1,0,1,1]
               }
t3 :: Transform
t3 = templateTransform { 
                tVariation     = affine af3
               , tXaos = [0,1,1,0,1,1,1]
               }

t4 :: Transform
t4 = templateTransform { 
                tVariation     = affine af4
               , tXaos = [1,1,0,1,1,0,1]
               }

t5 :: Transform
t5 = templateTransform { 
                tVariation     = affine af5
               , tXaos = [1,0,1,1,1,0,1]
               }

t6 :: Transform
t6 = templateTransform { 
                tVariation     = affine af6
               , tColorPosition = 1
               , tColorSpeed    = 0
               , tXaos = [1,1,1,0,0,1,1]
               }

t7 :: Transform
t7 = templateTransform { 
                tVariation     = affine af7
               , tColorPosition = 0
               , tColorSpeed    = 0.8
               , tXaos = [1,1,1,1,1,1,1]
               }

-- | exampleModel 42
exampleModel :: Model 
exampleModel = templateModel {
                       mTransforms = [t1,t2,t3,t4,t5,t6,t7]
                     , mGradient = grad
                     , mWidth = 800
                     , mHeight = 450
                     , mInnerIter = 256
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