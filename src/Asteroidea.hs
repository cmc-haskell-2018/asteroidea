{-|
Module      : Asteroidea
Description : Main module, starting simulation
Copyright   : Just Nothing
Stability   : in progress
-}

module Asteroidea where

import System.Random
import Graphics.Gloss
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Vector.Mutable
import Codec.Picture
import Types
import Data.Vector.Storable (unsafeToForeignPtr)
import Const
import Gradient
import Variations
import Model.Serpinski
import Core
import Plotter
--import Data.List
--import Debug.Trace
-- | Поехали!


run :: IO ()
run = do 
  genRand <-  newStdGen
  
  let startField =  initField mainModel
  let field = updateField mainModel startField $ calcFlame genRand mainModel
  let img = generateImage (fieldCellToPixel (mWidth mainModel) field) (mWidth mainModel) (mHeight mainModel)
  let pic = fromImageRGBA8 img
  
  savePngImage  "./pic.png" (ImageRGBA8  img) 
  animate window white (\_->pic)
  where       
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))

-- TO DO: разобраться с кашей из старых и новых типов
 
fromImageRGBA8
  :: Image PixelRGBA8
  -> Picture
fromImageRGBA8 
  Image { imageWidth = w, imageHeight = h, imageData = idat } =
  bitmapOfForeignPtr w h
                     (BitmapFormat TopToBottom PxRGBA)
                     ptr True
    where (ptr, _, _) = unsafeToForeignPtr idat



-- it's actually belongs to a post-coloring
-- | convert Field element to pixel 
fieldCellToPixel :: Int -> Field  -> Int -> Int -> PixelRGBA8
fieldCellToPixel width field x y =
  toPixel $  field  Vector.! (linearFieldIndex width (x,y))
  where
    toPixel (r, g, b, a) = PixelRGBA8 nr ng nb 255
     where
      nr = fromIntegral $ round $ (r/a)*255
      ng = fromIntegral $ round $ (g/a)*255
      nb = fromIntegral $ round $ (b/a)*255



