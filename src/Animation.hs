{-|
Module      : Animation
Description : Module that contains all Animation functions
Copyright   : Just Nothing
Stability   : in progress
-}
module Animation  where
import Types
import Core
import Plotter
import PostColoring
import Codec.Picture

animate
  :: Model -- m1
  -> Model -- m2
  -> Int   -- number of interpolation points
  -> Int   -- seed
  -> [Image PixelRGBA8]
animate m1 m2 num seed = let
  points1 = calcFlame m1 seed
  points2 = calcFlame m2 seed
  interCoeffs = map ( / ( fromIntegral num + 1) ) $ take num [1.0,2.0..]
  interpolated = map (interpolate points1 points2) interCoeffs
  allPoints = points1 : interpolated ++ [points2]
  fields = map (createField m1) allPoints
  generator = ( \ field
                -> generateImage
                     (fieldCellToPixel m1 field)
                     (mWidth m1)
                     (mHeight m1)
              )
  in map generator fields

-- | inerpolate two model results
interpolate
  :: [(Vec,Double,Transform)] -- from the first model
  -> [(Vec,Double,Transform)] -- from the second model
  -> Double                   -- interpolarion coeff. [0,1]
  -> [(Vec,Double,Transform)] -- result
interpolate l1 l2 coeff = zipWith combine l1 l2
  where
    combine ((x1,y1), c1, ptr) ((x2,y2), c2, _) = ((x3,y3),c3,ptr)
      where
        x3 = x1*(1-coeff) + x2*coeff
        y3 = y1*(1-coeff) + y2*coeff
        c3 = c1*(1-coeff) + c2*coeff