{-|
Module      : Plotter
Description : plotting pixels
Copyright   : Just Nothing
Stability   : in progress
-}

module Plotter (plot, linearFieldIndex) where
import Types
import qualified Gradient
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Vector.Mutable
import Control.Monad.ST (runST)

-- | отрисовка точки на поле
plot :: Model -> Field -> Cast -> Field
plot model field (v@(x,y), col)
  | inBounds = newField
  | otherwise = field
  where
    inBounds = control model x' y'
    shiftX, shiftY :: Double
    shiftX = scaleFactor * mShiftX model
    shiftY = scaleFactor * mShiftY model
    -- rotation in radian
    rotRad :: Double
    rotRad = (pi/360*) $ mRotation model
    -- sin / cos rotation multiplied on scaleFactor 
    sinTheta, cosTheta :: Double
    sinTheta = scaleFactor * sin rotRad
    cosTheta = scaleFactor * cos rotRad
    scaleFactor :: Double
    scaleFactor = mScale model /50
    x' = ((y*sinTheta + x*cosTheta) - shiftX)
    y' = ((y*cosTheta - x*sinTheta) - shiftY)
    setX = truncate ( ( x'+1) * (fromIntegral $ mWidth  model)/2  ) 
    setY = truncate ( (-y'+1) * (fromIntegral $ mHeight model)/2  )
-- -y because y-axis direction is opposite of row number
    coord = (setX, setY)
    linearCoord = linearFieldIndex (mWidth model) coord
    addedCol = Gradient.colorMap (mGradient model) col
    newField = runST $ do 
      mutableVector <- Vector.unsafeThaw field
      Vector.Mutable.modify  mutableVector (calcColour addedCol) linearCoord 
      updatedField <- Vector.unsafeFreeze mutableVector
      return updatedField
{-# INLINE plot #-}

linearFieldIndex :: Int -> (Int, Int) -> Int
linearFieldIndex w (i, j) = i + j * w
{-# INLINE linearFieldIndex #-}

-- | TODO alpha blending colours
calcColour :: (Double,Double,Double) -> Cell -> Cell
calcColour (r1,g1,b1) (r2, g2, b2, a) = ( (r2+r1), (g2+g1), (b2+b1), (a+1))
{-# INLINE calcColour #-}

-- | проверка что точка входит в поле
control :: Model -> Double -> Double -> Bool
-- не совсем верно - не учитывается зум и прочее
control m a b = not (cond halfX a || cond halfY b)
  where
    halfX = (fromIntegral $ mWidth m)/2
    halfY = (fromIntegral $ mHeight m)/2
    cond _ x = -- here was size
      isNaN x ||
      isInfinite x ||
      x <= - 1 ||
      x >=  1
{-# INLINE control #-}