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
import System.Random
import Codec.Picture

animate :: Model -- m1
        -> Model -- m2
        -> Int -- number of inerpolation points in between
        -> IO () -- not sure here
animate m1 m2 num = do 
  genRand <-  newStdGen
  let (seed, _) = next genRand
  let startField =  initField m1 -- we will use sizes and bckCol of m1
  let points1 = calcFlame m1 seed
  let points2 = calcFlame m2 seed
  let interCoeffs = map ( / ( fromIntegral num + 1) ) $ take num [1.0,2.0..]
  let interpolated = map (interpolate points1 points2) interCoeffs
  let all = points1 : interpolated ++ [points2]
  let fields = map (updateField m1 startField) all --updateField mainModel (calcFlame mainModel seed) startField
  let generator = (\ field -> generateImage (fieldCellToPixel m1 field) (mWidth m1) (mHeight m1)) 
  let images = map generator fields
  savePngImage  "./pic.png" (ImageRGBA8 $ head  images) -- just to make it compile 


-- | inerpolate two model results
interpolate :: [(Vec,Double,Transform)] -- from the first model
            -> [(Vec,Double,Transform)] -- from the second model
            -> Double -- interpolarion coeff. [0,1]
            -> [(Vec,Double,Transform)] -- result
interpolate l1 l2 coeff = zipWith combine l1 l2
  where
    combine ((x1,y1), c1, ptr) ((x2,y2), c2, _) = ((x3,y3),c3,ptr)
      where
      	x3 = x1*(1-coeff) + x2*coeff
      	y3 = y1*(1-coeff) + y2*coeff
      	c3 = c1*(1-coeff) + c2*coeff
