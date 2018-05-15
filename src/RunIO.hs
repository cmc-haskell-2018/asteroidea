{-|
Module      : RunIO
Description : Executing scripts, parsing args. IO ONLY HERE.
Copyright   : Just Nothing
Stability   : in progress
-}
module RunIO (parseArgs) where
import Core
import Plotter
import Const
import Types
import Codec.Picture
import Graphics.Gloss
import Model.Library as L
import Animation
import qualified Control.Monad as M
import Data.Vector.Storable (unsafeToForeignPtr)
import qualified Data.Vector.Unboxed as Vector
import Data.List
import Parser

-- Думаю перейти на cmdargs
-- | Разбор параметров командной строки.
parseArgs :: [String] -> (Int -> IO())
parseArgs commandArgs = case commandArgs of {
       (par:xs)      -> case par of
       "read"        -> parseRead          xs
       "pic"         -> parsePicture       xs
       "interpol"    -> parseInterpolation xs
       "anime"       -> parseAnimation     xs
       _             -> error "ParseError"
    ;  _             -> parseInterpolation []
                                            }

parseRead :: [String] -> Int -> IO()
parseRead (path:_) int = do
  contents <- readFile path
  let func = (savePngImage "./pic.png")
  let gen = (runPicture $ parseModel contents templateModel)
  let img = gen int
  let pic = fromImageRGBA8 $ convertRGBA8 img
  let window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY)) 
  func img
  Graphics.Gloss.animate window white (\_->pic)
parseRead _ _ = error "No file path given"
  -- where
  --   func = (savePngImage "./pic.png")
  --   gen = (runPicture $ parseModel $ words contents)
  --   img = gen int
  --   pic = fromImageRGBA8 $ convertRGBA8 img
  --   window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))

-- parsePicture _                   = runWindow
--                                    (savePngImage "./pic.png")
--                                    (runPicture $ L.anyModel)

-- -- | Запуск вывода и отображения в окне
-- runWindow :: (DynamicImage -> IO ()) -> (Int -> DynamicImage) -> Int -> IO ()
-- runWindow func gen int = do
--     func img
--     Graphics.Gloss.animate window white (\_->pic)
--   where
--     img = gen int
--     pic = fromImageRGBA8 $ convertRGBA8 img
--     window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))


-- | Разбор параметров анимации.
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

-- | Запуск анимации - интерполяция кадров, запись в gif.
runAnimation :: (Model, Model) -> Int -> String -> Int -> IO ()
runAnimation (m0,m1) num spath seed = do
  case  (
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

-- | Разбор аргументов интерполяции.
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

-- | Запуск интерполяции - сохранения набора картинок.
{-
sample
png- 1.2 MB
100- 1.6 MB
80 - 416 kB
50 - 223 kB
25 - 122 kB
10 - 50  kB
0  - 22  kB
-}
runInterpolation :: (Model, Model) -> Int -> Int -> IO ()
runInterpolation (m0,m1) num seed =
      M.sequence_
        (zipWith ($)
          (map (saveJpgImage 80) paths) -- Image PixelRGBA8
          (map (ImageRGBA8 . (generateImageM m1))
               (Animation.animate m0 m1 num seed)
          )
        )
  where
    paths = take (num+2) filenames
    filenames = map (++ ".jpg") $ map show [0::Int,1..]

-- | Разбор аргументов картинки.
parsePicture :: [String] -> (Int -> IO ())
parsePicture (name:file:qual:[]) = runWindow
                                   (saveJpgImage (read qual) (file++".jpg"))
                                   (runPicture $ L.findModel name)
parsePicture (name:file:[])
  | ".png" `isSuffixOf` file     = runWindow
                                   (savePngImage file)
                                   (runPicture $ L.findModel name)
  | ".jpg" `isSuffixOf` file     = runWindow
                                   (saveJpgImage 80 file)
                                   (runPicture $ L.findModel name)
parsePicture (name:[])           = runWindow
                                   (savePngImage "./pic.png")
                                   (runPicture $ L.findModel name)
parsePicture _                   = runWindow
                                   (savePngImage "./pic.png")
                                   (runPicture $ L.anyModel)

-- | Запуск вывода и отображения в окне
runWindow :: (DynamicImage -> IO ()) -> (Int -> DynamicImage) -> Int -> IO ()
runWindow func gen int = do
    func img
    Graphics.Gloss.animate window white (\_->pic)
  where
    img = gen int
    pic = fromImageRGBA8 $ convertRGBA8 img
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))

-- | Описание расчёта картинки, вывод DynamicImage.
runPicture
  :: Model -> Int -> DynamicImage
runPicture model seed = let
    field = createField model $ calcFlame model seed 
    img = generateImageM model field
  in ImageRGBA8 img

-- | convertation from DynamicImage to Picture
fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 
  Image { imageWidth = w, imageHeight = h, imageData = idat } =
  bitmapOfForeignPtr w h
                     (BitmapFormat TopToBottom PxRGBA)
                     ptr True
    where (ptr, _, _) = unsafeToForeignPtr idat
-- | image generator, pair model-field is determing its behaving
generateImageM :: Model -> Field -> Image PixelRGBA8
generateImageM m1 field = generateImage
                     (fieldCellToPixel m1 field)
                     (mWidth m1)
                     (mHeight m1)
-- | scanner for field, returns PixelRGBA8
fieldCellToPixel :: Model -> Field  -> Int -> Int -> PixelRGBA8
fieldCellToPixel m field x y =
  toPixel $  field  Vector.! (linearFieldIndex m (x,y))
  where
    toPixel (r, g, b, a) = PixelRGBA8 nr ng nb 255
     where
      nr = fromInteger $ round $ (r/a)*255
      ng = fromInteger $ round $ (g/a)*255
      nb = fromInteger $ round $ (b/a)*255