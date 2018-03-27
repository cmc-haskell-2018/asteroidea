{-|
Module      : Asteroidea
Description : Main module, starting simulation
Copyright   : Just Nothing
Stability   : in progress
-}

--{-# OPTIONS_GHC -XStrict #-}
module Asteroidea where

import System.Random
import Graphics.Gloss
import Data.Matrix hiding(trace)
import Data.List
import Codec.Picture
import Types
import Data.Vector.Storable (unsafeToForeignPtr)
import Const
import Variations
--import Debug.Trace
-- | Поехали!
run :: IO ()
run = do 
  -- Генератор случайных чисел, начальная инициализация
  genRand <-  newStdGen
  
  let field = calcFlame genRand mainModel 
  let img = generateImage (fieldCellToPixel $! field)  (width mainModel) (height mainModel)
  let pic = fromImageRGBA8 img
  
  savePngImage  "./pic.png" $! (ImageRGBA8  img) 
  display window white $! pic

  
  where   
    --getter = getWorldPoint
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
   -- update = updateWorld
-- window -- FullScreen
fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 (Image { imageWidth = w, imageHeight = h, imageData = idat }) =
  bitmapOfForeignPtr w h
                     (BitmapFormat TopToBottom PxRGBA)
                     ptr True
    where (ptr, _, _) = unsafeToForeignPtr idat
-- | Act of Creation
-- создание мира

-- TODO StdGen Handling
-- в текущий момент тут нет никакого рандома

-- | Calculate whole fractal
calcFlame :: StdGen ->  Model -> Field
calcFlame gen model = fst $ foldl' (calcPath model) startField pointList
  where
    sizeX = width model
    sizeY = height model
    startField = (matrix sizeX sizeY initFunction, gen)
    initFunction = \_ -> Cell 0 0 0 0  -- По хорошему цвет фона должен быть в модели
    pointList = take outerIter busPointList -- лист с точками что будем обсчитывать
    outerIter = 21845 -- внешний цикл, 
--(b -> a -> b) -> b -> t a -> b

-- | Calculate and plot Path of one point from [-1,1]^2
calcPath ::  Model->(Field,StdGen)->Vec->(Field,StdGen)
calcPath  model (field,gen) !vec = (foldl' (plot model) field path, lastGen)
  where
    start = (GVec gen vec, 0.5) -- CastGen
    infPath = iterate (calcOne model) start -- весь путь точки
    path = drop 20 $! take 25 $! infPath -- 30 - внутренний цикл
    lastGen = vgGen $ fst $ last path

-- | Calculate one point and color
calcOne :: Model -> CastGen -> CastGen
calcOne model ( GVec gen v, col) = (newGVec, newCol)
  where
    (ptr , newGen) = randomR (0, (length $ tranforms model) -1 ) gen
    tranform = tranforms model !! ptr    
    newGVec = calcVariation (variation tranform) (GVec newGen v)
    newCol = (col + (colorPosition tranform))/2 -- To Do speed
  {- 
-- | Вывод поля на экран playField
-- не совсем верно - зум и поворто должны производится до нанесения на поле, если же после, то это приводит к потере части изображения
getWorldPoint :: World -> Int -> Int -> Cell 
getWorldPoint bnw (i,j)
  | flag = getElem trrI trrJ (mugenga bnw)
  | otherwise = backGrCol
  where
    x = j*sinTheta + i*cosTheta
    y = j*cosTheta - i*sinTheta
    trrI = round (x*halfX - shiftX)
    trrJ = round (y*halfY - shiftY)
    flag = not (cond sizeX trrI|| cond sizeY trrJ)
    cond size a = a < 1 || a > size
-}
-- отрисовка точки на поле
plot ::Model -> Field -> CastGen -> Field
plot model !field !(GVec _ v@(x,y), col) | inBounds = newField
                               | otherwise = field
  where
    inBounds = control model v
    setX = 1 + truncate ( (x+1) * (fromIntegral $ width model)/2  ) 
    setY = 1 + truncate ( (-y+1) * (fromIntegral $ height model)/2  ) -- | -y because y-axis direction is opposite of row number
    coord = (setX, setY)
    colour = calcColour col  (field ! coord) -- установка $! здесь приводит к неогранченному росту потребления памяти
    newField = setElem colour coord $! field


control :: Model -> (Double,Double) -> Bool -- не совсем верно - не учитывается зум и прочее
control m !(a,b) = not (cond halfX a || cond halfY b)
  where
    halfX = (fromIntegral $ width m)/2
    halfY = (fromIntegral $ height m)/2
    cond _ x = -- here was size
      isNaN x ||
      isInfinite x ||
      x <= - 1 ||
      x >=  1
-- | TODO alpha blending colours
calcColour :: Double -> Cell -> Cell
calcColour _ _ = Cell 1 0 0 1 -- заглушка

fieldCellToPixel ::  Field -> Int -> Int -> PixelRGBA8
fieldCellToPixel field x y = toPixel $ getElem (x+1) (y+1) field 
  where
    toPixel (Cell r g b a )= PixelRGBA8 nr ng nb 255
     where
      nr = fromIntegral $ round $ (r/a)*255
      ng = fromIntegral $ round $ (g/a)*255
      nb = fromIntegral $ round $ (b/a)*255