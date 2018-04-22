{-|
Module      : Main
Description : Main module, running script. IO ONLY here.
Copyright   : Just Nothing
Stability   : in progress
-}
module Main where
import Core
import Plotter
import Const
import Types
import System.Random (newStdGen, next)
import Codec.Picture
import Graphics.Gloss
import Model.Library as L
import Animation
import qualified System.Environment as S
import qualified Control.Monad as M
import Data.Vector.Storable (unsafeToForeignPtr)
import qualified Data.Vector.Unboxed as Vector

main :: IO ()
main = do
  genRand <-  newStdGen
  let (seed, _) = next genRand
  commandArgs <- S.getArgs
  case commandArgs of {
       []            -> parseInterpolation [] seed
    ;  (par:xs)      -> case par of
       "pic"         -> parsePicture       xs seed
       "interpol"    -> parseInterpolation xs seed
       "anime"       -> parseAnimation     xs seed
       _             -> error "ParseError"
                      }
  putStrLn "For the Great Good!"

-- | parse animation args
parseAnimation :: [String] -> (Int -> IO ())
parseAnimation (name0:name1:file:num:[])
  = runAnimation
       (  (L.findModel name0) , (L.findModel name1)  )
       (read num)
       file
parseAnimation (name0:name1:file:[])
  = runAnimation
       (  (L.findModel name0) , (L.findModel name1)  )
       60
       file
parseAnimation (file:num:[])
  = runAnimation (L.anyPair) (read num) file
parseAnimation (file:[])
  = runAnimation (L.anyPair) 30 file
parseAnimation _
  = runAnimation (L.anyPair) 1 "pic.gif"
-- | running animation into gif
runAnimation :: (Model, Model) -> Int -> String -> Int -> IO ()
runAnimation (m0,m1) num spath seed = do
  case  (
     -- FilePath -> GifDelay -> GifLooping -> [Image PixelRGB8] -> Either String (IO ()) 
     writeGifAnimation spath 100 LoopingForever 
     (map (convertRGB8 . ImageRGBA8)
             (map (generateImageM m0)
                  (Animation.animate m0 m1 num seed)
             )
     )
        )
    of
    Left  err0 -> putStrLn err0
    Right save -> save

-- | parse interpolation args
parseInterpolation :: [String] -> (Int -> IO ())
parseInterpolation (name0:name1:num:[]) =
     runInterpolation
     (  (L.findModel name0) , (L.findModel name1)  )
     (read num)
parseInterpolation (name0:name1:[]) =
     runInterpolation
     (  (L.findModel name0) , (L.findModel name1)  )
     3
parseInterpolation _  = runInterpolation (L.anyPair) 1

-- | running animation into set of pictures
runInterpolation :: (Model, Model) -> Int -> Int -> IO ()
runInterpolation (m0,m1) num seed =
          M.zipWithM_
          (savePngImage) paths -- Image PixelRGBA8
          (map ImageRGBA8
             (map (generateImageM m1)
                  (Animation.animate m0 m1 num seed)
             )
          )
  where
    paths = take (num+2) filenames
    filenames = map (++ ".png") $ map show [0::Int,1..]

-- | parse picture args
parsePicture :: [String] -> (Int -> IO ())
parsePicture (name:file:[]) = runPicture (L.findModel name) file
parsePicture (name:[])      = runPicture (L.findModel name) "./pic.png"
parsePicture _              = runPicture (L.anyModel)       "./pic.png"

-- | running one picture in window
runPicture :: Model -> String -> Int -> IO ()
runPicture model file seed = do
  let field = createField model  $ calcFlame model seed 
  let img = generateImageM model field
  savePngImage file (ImageRGBA8  img)
  let pic = fromImageRGBA8 img
  Graphics.Gloss.animate window white (\_->pic)
  where
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))

fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 
  Image { imageWidth = w, imageHeight = h, imageData = idat } =
  bitmapOfForeignPtr w h
                     (BitmapFormat TopToBottom PxRGBA)
                     ptr True
    where (ptr, _, _) = unsafeToForeignPtr idat

generateImageM :: Model -> Field -> Image PixelRGBA8
generateImageM m1 field = generateImage
                     (fieldCellToPixel m1 field)
                     (mWidth m1)
                     (mHeight m1)

fieldCellToPixel :: Model -> Field  -> Int -> Int -> PixelRGBA8
fieldCellToPixel m field x y =
  toPixel $  field  Vector.! (linearFieldIndex m (x,y))
  where
    toPixel (r, g, b, a) = PixelRGBA8 nr ng nb 255
     where
      nr = fromInteger $ round $ (r/a)*255
      ng = fromInteger $ round $ (g/a)*255
      nb = fromInteger $ round $ (b/a)*255