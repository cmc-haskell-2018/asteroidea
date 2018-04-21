module Main where


import Core
import Plotter
import PostColoring
import Const
import Types
import System.Random
import Codec.Picture
import Graphics.Gloss
import Model.Serpinski as M0
import Model.Serpinski0 as M1
import Animation
import qualified System.Environment as S

main :: IO ()
main = do
  genRand <-  newStdGen
  let (seed, _) = next genRand 
  commandArgs <- S.getArgs
  case commandArgs of {
       []            -> run
    ;  (par:[])      -> case par of
       "pic"         -> run
       "interpol"    -> runAnim seed
       _             -> run
    ;  _             -> runAnim seed
                      }

runAnim :: Int -> IO ()
runAnim seed = sequence_
        $ zipWith (\p im -> savePngImage p $ ImageRGBA8 im) paths
        $ Animation.animate M0.mainModel M1.mainModel 60 seed
  where
    num = 60
    paths = take (num+1) filenames
    filenames = concat [l1,l2,l3]
      where
        l1 = map (\s -> "0"++"0"++ show s) $ [0,9::Int]
        l2 = map (\s -> "0"     ++ show s) $ [10,99::Int]
        l3 = map show [100::Int,101..]

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