{-|
Module      : PostColoring
Description : post coloring of the field
Copyright   : Just Nothing
Stability   : OLEG
-}
{--
Для базы необходимы:
Data для хранения всех параметров постобработки - гамма\вайбранси и прочие простые параметры, позже ргб кривые
Собственно функция что принимает эти параметры, поле и выдает картинку/поле :: PostColorParams->Field->Field
--}
module PostColoring where


import Graphics.Gloss
import Types
import Codec.Picture
import Graphics.Gloss
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Storable (unsafeToForeignPtr)

type PostColorParam = Double
-- |...
--параметр обработки
type PostColorParams = [PostColorParam]
--все параметры обработки

postColoring :: PostColorParams -> Field -> Field
postColoring _ f = f
--главная функция постобработки

fromImageRGBA8 :: Image PixelRGBA8 -> Picture
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
