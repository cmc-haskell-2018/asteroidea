module Main where

import Model.Square
import Core
import Plotter
import PostColoring
import Const
import Types
import System.Random
import Codec.Picture
import Graphics.Gloss

main :: IO ()
main = run

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