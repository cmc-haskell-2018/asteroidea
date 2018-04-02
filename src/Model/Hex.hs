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
t1 = Transform { tName          = "t1"
               , tVariation     = af1
               , tWeight        = 1
               , tColorPosition = 1
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t2 :: Transform
t2 = Transform { tName          = "t1"
               , tVariation     = af2
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }
t3 :: Transform
t3 = Transform { tName          = "t1"
               , tVariation     = af3
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t4 :: Transform
t4 = Transform { tName          = "t1"
               , tVariation     = af4
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t5 :: Transform
t5 = Transform { tName          = "t1"
               , tVariation     = af5
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t6 :: Transform
t6 = Transform { tName          = "t1"
               , tVariation     = af6
               , tWeight        = 1
               , tColorPosition = 1
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

t7 :: Transform
t7 = Transform { tName          = "t1"
               , tVariation     = af7
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0.8
               , tOpacity       = 1
               , tXaos          = []
               }

-- | exampleModel 42
exampleModel :: Model 
exampleModel = Model { mName = "42"
                     , mTransforms = [t1,t2,t3,t4,t5,t6,t7]
                     , mCamera = Nothing
                     , mGradient = grad
                     , mWidth = 1024
                     , mHeight = 1024
                     , mScale = 50
                     , mRotation = 0
                     }
  where
grad = paletteToDouble "00003A00004000003D00002500001C0000250D00433D00947037DFB8A0FFCABEFFCABEFFCDCAFFE2DFFFD0D3FF9A7FFF8573FF9A91FFDFE2FFFFFFFFFFFFFFF1CAFF8834FA6A19EB8E4CFFF1E5FFFFFFFFFFFDFFA661FF6410E522008E2B008E583DCA9497FF8888FF3D1CC400004600001C0100491000611C00793700944C00AF9161FFEEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD9FFDFA6FF5E22BB37008200004900004300005200006D00007013007F4300AC8B6DFFF4EBFFFFFFFFFFFFFFFFFFFFE8D0FFD6CDFFE8DCFFF1EBFFBEA3FFA682FFB288FFEBBEFFFFC4FFB897FF5837FF0000BB00006D00002E00002E00002500003A00004000003D00002500001C0000250D00433D00947037DFB8A0FFCABEFFCABEFFCDCAFFE2DFFFD0D3FF9A7FFF8573FF9A91FFDFE2FFFFFFFFFFFFFFF1CAFF8834FA6A19EB8E4CFFF1E5FFFFFFFFFFFDFFA661FF6410E522008E2B008E583DCA9497FF8888FF3D1CC400004600001C0100491000611C00793700944C00AF9161FFEEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD9FFDFA6FF5E22BB37008200004900004300005200006D00007013007F4300AC8B6DFFF4EBFFFFFFFFFFFFFFFFFFFFE8D0FFD6CDFFE8DCFFF1EBFFBEA3FFA682FFB288FFEBBEFFFFC4FFB897FF5837FF0000BB00006D00002E00002500003A00004000003D00002500001C0000250D00433D00947037DFB8A0FFCABEFFCABEFFCDCAFFE2DFFFD0D3FF9A7FFF8573FF9A91FFDFE2FFFFFFFFFFFFFFF1CAFF8834FA6A19EB8E4CFFF1E5FFFFFFFFFFFDFFA661FF6410E522008E2B008E583DCA9497FF8888FF3D1CC400004600001C0100491000611C00793700944C00AF9161FFEEFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD9FFDFA6FF5E22BB37008200004900004300005200006D00007013007F4300AC8B6DFFF4EBFFFFFFFFFFFFFFFFFFFFE8D0FFD6CDFFE8DCFFF1EBFFBEA3FFA682FFB288FFEBBEFFFFC4FFB897FF5837FF0000BB00006D00002E000025"