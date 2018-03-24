{-|
Module      : Asteroidea
Description : Main module, starting simulation
Copyright   : Just Nothing
Stability   : in progress
-}
module Asteroidea where

import System.Random
import Graphics.Gloss
import Data.Matrix
import Data.List
import Codec.Picture
import Types
import ClassField
import Data.Vector.Storable (unsafeToForeignPtr)
import Const
import Variations
-- | Поехали!
run :: IO ()
run = do 
  -- Генератор случайных чисел, начальная инициализация
  genRand <- newStdGen
  let field = calcFlame genRand mainModel
  let img = generateImage (fieldCellToPixel $! field) (width mainModel) (height mainModel)
  let pic = fromImageRGBA8 $! img
  savePngImage "./pic.png" $! (ImageRGBA8  img) 
  display window white $! pic
  
  where   
    --getter = getWorldPoint
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))
   -- update = updateWorld
-- window -- FullScreen
fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 (Image { imageWidth = w, imageHeight = h, imageData = id }) =
  bitmapOfForeignPtr w h
                     (BitmapFormat TopToBottom PxRGBA)
                     ptr True
    where (ptr, _, _) = unsafeToForeignPtr id
-- | Act of Creation
-- создание мира

-- TODO StdGen Handling
-- в текущий момент тут нет никакого рандома

-- | Calculate whole fractal
calcFlame :: StdGen ->  Model -> Field
calcFlame gen model = foldl' (calcPath gen model) startField pointList
  where
    sizeX = width model
    sizeY = height model
    startField = matrix sizeX sizeY initFunction
    initFunction = \(a,b) -> Cell 0 0 0 0  -- По хорошему цвет фона должен быть в модели
    pointList = take outerIter busPointList -- лист с точками что будем обсчитывать
    outerIter = 21845 -- внешний цикл, 
--(b -> a -> b) -> b -> t a -> b

-- | Calculate and plot Path of one point from [-1,1]^2
calcPath :: StdGen -> Model->Field->Vec->Field
calcPath gen model field !vec = foldl' (plot model) field path
  where
    start = (GVec gen vec, 0.5) -- CastGen
    infPath = iterate (calcOne model) start -- весь путь точки
    path = drop 20 $! take 21 $! infPath -- 30 - внутренний цикл

-- | Calculate one point and color
calcOne :: Model -> CastGen -> CastGen
calcOne _ !c = c
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
plot model !field !(GVec gen v@(x,y), col) | inBounds = newField
                               | otherwise = field
  where
    inBounds = control model v
    setX = 1 + round ( (x+1) * (fromIntegral $ width model)/2  ) 
    setY = 1 + round ( (-y+1) * (fromIntegral $ height model)/2  )
    coord = (setX, setY)
    colour = calcColour col (field ! coord)
    newField = setElem colour coord field


control :: Model -> (Double,Double) -> Bool -- не совсем верно - не учитывается зум и прочее
control m !(a,b) = not (cond halfX a || cond halfY b)
  where
    halfX = (fromIntegral $ width m)/2
    halfY = (fromIntegral $ height m)/2
    cond size x =
      isNaN x ||
      isInfinite x ||
      x < - 1 ||
      x >   1
-- | TODO alpha blending colours
calcColour :: Double -> Cell -> Cell
calcColour _ _ = Cell 1 0 0 1 -- заглушка

fieldCellToPixel ::  Field -> Int -> Int -> PixelRGBA8
fieldCellToPixel field x y = toPixel $ getElem (x+1) (y+1) field 
  where
    toPixel (Cell r g b a )= PixelRGBA8 nr ng nb 255
     where
      nr = fromIntegral $ round $ r*255
      ng = fromIntegral $ round $ g*255
      nb = fromIntegral $ round $ b*255
