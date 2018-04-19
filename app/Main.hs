module Main where


import Core
import Plotter
import PostColoring
import Const
import Types
import System.Random
import Codec.Picture
import Graphics.Gloss
import Model.Serpinski

main :: IO ()
main = run

run :: IO ()
run = do 
  genRand <-  newStdGen
  
  let startField =  initField mainModel
  let field = updateField mainModel (calcFlame genRand mainModel) startField 
  let img = generateImage (fieldCellToPixel mainModel field) (mWidth mainModel) (mHeight mainModel)
  let pic = fromImageRGBA8 img
  
  savePngImage  "./pic.png" (ImageRGBA8  img) 
  animate window white (\_->pic)
  where       
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))