{-|
Module      : Plotter
Description : plotting pixels
Copyright   : Just Nothing
Stability   : in progress
-}

module Plotter (initField, updateField, linearFieldIndex) where
import Types
import Core
import Data.Maybe
import qualified Gradient
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Vector.Mutable
import Control.Monad.ST (runST)


-- | Initialize field
initField :: Model -> Field
initField m = Vector.generate (sizeX*sizeY) initFunction
  where
    sizeX = mWidth m
    sizeY = mHeight m
    initFunction = mBackgroundColour m  

-- | Add points to the field
updateField :: Model -> Field -> [CastGen]-> Field
updateField m oldField points = foldl (plot m) oldField finalestPoints 
 where
  finalPoints | isNothing $ mFinal m  = points
              | otherwise             = map (applyFinal m) points
  finalestPoints = map (\ (GVec g vec, c,i) -> (GVec g (applyCamera m vec), c,i)) finalPoints
 --finalestPoints = map (applyCamera m) finalPoints

{-
-- | отрисовка точки на поле
plot :: Model -> Field -> CastGen -> Field
plot model field ((GVec g v@(x,y)), col, ptr)
  | inBounds = newField
  | otherwise = field
  where
    inBounds = control model x y
    {-
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
    
    -}
    setX = truncate ( ( x+1) * (fromIntegral $ mWidth  model)/2  ) 
    setY = truncate ( (-y+1) * (fromIntegral $ mHeight model)/2  )
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
-}
applyFinal :: Model -> CastGen -> CastGen
applyFinal (Model {mFinal = Just final}) point = calcOne final point

applyCamera :: Model -> Vec -> Vec
applyCamera m (x,y) = (scalex,scaleY)
  where
    (shiftX, shiftY) = (x+ mShiftX m, y+ mShiftY m)
    rotRad = (2*pi/360*) $ mRotation m
    sinT = sin rotRad
    cosT = cos rotRad
    (rotX, rotY) = ( shiftX*cosT-shiftY*sinT, shiftY*cosT+shiftX*sinT)
    (scalex,scaleY) =(rotX * mScale m,rotY * mScale m )

-- | отрисовка точки на поле
plot :: Model -> Field -> CastGen -> Field
plot model field ((GVec g v@(x,y)), col, ptr)
  | inBounds = newField
  | otherwise = field
  where
    inBounds = control model v
    setX = truncate ( ( x+1) * (fromIntegral $ mWidth  model)/2  ) 
    setY = truncate ( (-y+1) * (fromIntegral $ mHeight model)/2  )
-- -y because y-axis direction is opposite of row number
    coord = (setX, setY)
    linearCoord = linearFieldIndex (mWidth model) coord
    addedCol = Gradient.colorMap (mGradient model) col
    newField = runST $ do 
      mutableVector <- Vector.unsafeThaw field
      Vector.Mutable.modify  mutableVector (calcColour addedCol) linearCoord 
      updatedField <- Vector.unsafeFreeze mutableVector
      return updatedField


-- | проверка что точка входит в поле
control :: Model -> (Double,Double) -> Bool
-- не совсем верно - не учитывается зум и прочее
control m (a,b) = not (cond halfX a || cond halfY b)
  where
    halfX = (fromIntegral $ mWidth m)/2
    halfY = (fromIntegral $ mHeight m)/2
    cond _ x = -- here was size
      isNaN x ||
      isInfinite x ||
      x <= - 1 ||
      x >= 1

{-# INLINE control #-}


-- | TODO alpha blending colours
calcColour :: (Double,Double,Double) -> Cell -> Cell
calcColour (r1,g1,b1) (r2, g2, b2, a) = ( (r2+r1), (g2+g1), (b2+b1), (a+1))
{-# INLINE calcColour #-}

{-
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
-}