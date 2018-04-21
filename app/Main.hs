{-|
Module      : Main
Description : Main module, running script. IO ONLY here.
Copyright   : Just Nothing
Stability   : in progress
-}
module Main where
import Core
import Plotter
import PostColoring
import Const
import Types
import System.Random
import Codec.Picture
import Graphics.Gloss
import Model.Serpinski as Serpinski
import Model.Library as L
import Animation
import qualified System.Environment as S

main :: IO ()
main = do
  genRand <-  newStdGen
  let (seed, _) = next genRand 
  commandArgs <- S.getArgs
  case commandArgs of {
       []            -> run (L.anyModel) seed
    ;  (par:[])      -> case par of
       "pic"         -> run (L.anyModel) seed
       "interpol"    -> runAnim seed
       _             -> run (L.anyModel) seed
    ;  (par:sec:[])  -> case par of
       "pic"         -> run (L.findModel sec) seed
       "interpol"    -> runAnim seed
       _             -> runAnim seed
    ;  _             -> run (L.anyModel) seed
                      }
  putStrLn "For the Great Good!"

runAnim :: Int -> IO ()
runAnim seed = sequence_
        $ zipWith (\p im -> savePngImage p $ ImageRGBA8 im) paths
        $ Animation.animate m0 m1 num seed
  where
    (m0:m1:[]) = Serpinski.listModel
    num = 3
    paths = take (num+2) filenames
    filenames = concat [l1,l2,l3]
      where
        l1 = map (\s -> "0"++"0"++ (show s) ++ ".png") $ [0::Int,1..9]
        l2 = map (\s -> "0"     ++ (show s) ++ ".png") $ [10::Int,11..99]
        l3 = map (++ ".png") $ map show [100::Int,101..]

run :: Model -> Int -> IO ()
run model seed = do
  let field = createField model  $ calcFlame model seed 
  let img = generateImage
              (fieldCellToPixel model field)
              (mWidth model)
              (mHeight model)
  let pic = fromImageRGBA8 img
  savePngImage  "./pic.png" (ImageRGBA8  img) 
  Graphics.Gloss.animate window white (\_->pic)
  where
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
