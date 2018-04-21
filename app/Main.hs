module Main where


import Core
import Plotter
import PostColoring
import Const
import Types
import Parser
import System.Random
import Codec.Picture
import Graphics.Gloss
import qualified Model.Serpinski as M1
import qualified Model.Serpinski0 as M0

import Animation

main :: IO ()
main = run

runAnim :: IO ()
runAnim = Animation.animate M0.mainModel M1.mainModel 60

run :: IO ()
run = do 
  genRand <-  newStdGen
  let (seed, _) = next genRand 
  let field = createField M1.mainModel  $ calcFlame M1.mainModel seed 
  let img = generateImage (fieldCellToPixel M1.mainModel field) (mWidth M1.mainModel) (mHeight M1.mainModel)
  let pic = fromImageRGBA8 img
  
  savePngImage  "./pic.png" (ImageRGBA8  img) 
  Graphics.Gloss.animate window white (\_->pic)
  where       
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
