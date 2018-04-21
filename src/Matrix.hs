{-|
Module      : Matrix
Description : matrix operations
Copyright   : Just Nothing
Stability   : in progress
-}
module Matrix where

-- | Матрицы афинных преобразований
{- @
 [ xx xy ox ]
 [ yx yy oy ]
 [ 0  0  1  ]
   @
-}
data AffineMatrix = AffineMatrix {
  xx, xy :: Double,
  yx, yy :: Double,
  ox, oy :: Double
} deriving(Show)

-- DO NOT TOUCH THIS
-- | Вращение
{- @
 [cos theta, - sin theta, 0]   [ xx xy ox ]
 [sin theta,   cos theta, 0] x [ yx yy oy ]
 [        0,           0, 1]   [ 0  0  1  ]
   @
-}
rotate :: Double -> AffineMatrix -> AffineMatrix
{-# INLINE[~1] rotate #-}
rotate angle (AffineMatrix xx0 xy0 yx0 yy0 a b) =
  AffineMatrix xx1 xy1 yx1 yy1 a b
  where
    xx1    = ff xx0 (-yx0)
    xy1    = ff xy0 (-yy0)
    yx1    = ff yx0   xx0
    yy1    = ff yy0   xy0
    ff x y = cosA*x + sinA*y
    angle' = angle * pi / 180
    sinA = sin angle'
    cosA = cos angle'

scale :: Double -> AffineMatrix ->  AffineMatrix
scale coeff am = scaleX coeff $ scaleY coeff am

scaleX :: Double -> AffineMatrix ->  AffineMatrix
scaleX coeff am = am { xx = coeff * xx am, xy = coeff * xy am }

scaleY :: Double -> AffineMatrix ->  AffineMatrix
scaleY coeff am = am { yy = coeff * yy am, yx = coeff * yx am }

translate :: (Double,Double) -> AffineMatrix -> AffineMatrix
translate (x, y) am = am { ox = x + ox am , oy = y + oy am} 

-- | стандартная уменьшающая матрица, шаблон
stdMatrix :: AffineMatrix
{-# INLINE stdMatrix #-}
stdMatrix = AffineMatrix 0.5 0 0 0.5 0 0

-- | Произведение матриц
{- @
[xx0 xy0 a0] [xx1 xy1 a1] [xx0*xx1+xy0*yx1,xx0*xy1+xy0*yy1,xx0*a1+xy0*b1+a0]
[yx0 yy0 b0]x[yx1 yy1 b1]=[yx0*xx1+yy0*yx1,yx0*xy1+yy0*yy1,yx0*a1+yy0*b1+b0]
[0   0   1 ] [ 0   0  1 ] [       0       ,       0       ,        1       ]
   @
-}
multiplyM :: AffineMatrix -> AffineMatrix -> AffineMatrix
{-# INLINABLE[2] multiplyM #-}
multiplyM
  (AffineMatrix xx0 xy0 yx0 yy0 a0 b0)
  (AffineMatrix xx1 xy1 yx1 yy1 a1 b1)
  =
   AffineMatrix xx2 xy2 yx2 yy2 a2 b2
  where
    xx2     = ffx xx1 yx1
    xy2     = ffx xy1 yy1
    a2      = (ffx a1 b1) + a0
    yx2     = ffy xx1 yx1
    yy2     = ffy xy1 yy1
    b2      = (ffy a1 b1) + b0
    ffx x y = xx0*x+xy0*y
    ffy x y = yx0*x+yy0*y

-- | Матрица вращения
{- @
 [cos theta, - sin theta, 0]   [ xx xy ox ]
 [sin theta,   cos theta, 0] x [ yx yy oy ]
 [        0,           0, 1]   [ 0  0  1  ]
   @
-}
rotateMatrix :: Double -> AffineMatrix
{-# INLINABLE[2] rotateMatrix #-}
rotateMatrix angle = AffineMatrix cosA (-sinA) sinA cosA 0 0
  where
    angle' = angle * pi / 180
    sinA = sin angle'
    cosA = cos angle'

{-# RULES
"multiplyM/rotateM"[~1] forall f g. multiplyM (rotateMatrix f) g = rotate f g
#-}
