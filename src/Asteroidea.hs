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
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Vector.Mutable
import Data.List
import Codec.Picture
import Types
import Data.Vector.Storable (unsafeToForeignPtr)
import Const
import Gradient
import Variations
--import Debug.Trace
-- | Поехали!
type Cell = (Double,Double,Double,Double)
type Field = Vector.Vector Cell
run :: IO ()
run = do 
  -- Генератор случайных чисел, начальная инициализация
  genRand <-  newStdGen
  
  let startField =  initField mainModel
  let field = updateField mainModel startField $ calcFlame genRand mainModel
  let img = generateImage (fieldCellToPixel (mWidth mainModel) field)  (mWidth mainModel) (mHeight mainModel)
  let pic = fromImageRGBA8 img
  
  savePngImage  "./pic.png" (ImageRGBA8  img) 
  display window white pic  
  where       
    window = (InWindow "Just Nothing" (winX, winY) (startPosX, startPosY))

-- TO DO: разобраться с кашей из старых и новых типов
 
fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 (Image { imageWidth = w, imageHeight = h, imageData = idat }) =
  bitmapOfForeignPtr w h
                     (BitmapFormat TopToBottom PxRGBA)
                     ptr True
    where (ptr, _, _) = unsafeToForeignPtr idat

-- | Add points to the field
updateField :: Model -> Field -> [(Vec,Double)]->Field
updateField m oldField xs = foldl (plot m) oldField xs 

-- | Initialize field
initField :: Model -> Field
initField m = Vector.generate (sizeX*sizeY) initFunction
  where
    sizeX = mWidth m
    sizeY = mHeight m
    initFunction = \_ -> (0,0,0,0)  -- По хорошему цвет фона должен быть в модели


-- | Calculate whole fractal
calcFlame :: StdGen ->  Model -> [(Vec,Double)]
calcFlame gen model = concat $ map (calcPath model) pointList
  where
    --(g1,g2)= split gen    
    --pointList = take outerIter busPointList -- лист с точками что будем обсчитывать
    pointList = take outerIter (randBUSlist gen) -- лист с точками что будем обсчитывать
    outerIter = 21845 -- внешний цикл, 
--(b -> a -> b) -> b -> t a -> b

-- | Calculate and plot Path of one point
calcPath ::  Model->Vec->[(Vec,Double)]
calcPath  model vec@(x,y) = cleanPath
  where
    gen =  mkStdGen $ floor (10000 * (x+y))
    start = (GVec gen vec, 0.5) -- CastGen
    infPath = iterate (calcOne model) start -- весь путь точки
    path = drop 20 $ take 200 $ infPath -- 300 - внутренний цикл
    cleanPath = map convertCast path

-- | Convert CastGen to (Vec,Double)
convertCast :: CastGen -> (Vec,Double)
convertCast (GVec _ v , col) = ( v , col)


-- | Calculate one point and color
calcOne :: Model -> CastGen -> CastGen
calcOne model ( GVec gen v, col) = (newGVec, newCol)
  where
    (ptr , newGen) = randomR (0, (length $ mTransforms model) -1 ) gen
    transform = mTransforms model !! ptr    
    newGVec = calcVariation (tVariation transform) (GVec newGen v)
    speed = tColorSpeed transform
    newCol = ( col *  (1 + speed) + (tColorPosition transform) * (1 - speed) )/2 

-- отрисовка точки на поле
plot :: Model -> Field -> (Vec,Double) -> Field
plot model field (v@(x,y), col)  | inBounds = newField
                                 | otherwise = field
  where
    inBounds = control model v
    setX = truncate ( (x+1) * (fromIntegral $ mWidth model)/2  ) 
    setY = truncate ( (-y+1) * (fromIntegral $ mHeight model)/2  ) -- -y because y-axis direction is opposite of row number
    coord = (setX, setY)
    linearCoord = linearFieldIndex (mWidth model) coord
    addedCol = Gradient.colorMap (mGradient model) col
    --colour = calcColour addedCol $ (Vector.!) field linearCoord -- установка $! здесь приводит к неогранченному росту потребления памяти
    --Field = field Vector.// [(linearCoord, colour)]
    
    newField = runST $ do -- setElem colour coord $! field
      mutableVector <- Vector.unsafeThaw field
      Vector.Mutable.modify  mutableVector (calcColour addedCol) linearCoord 
      updatedField <- Vector.unsafeFreeze mutableVector
      return updatedField
    

linearFieldIndex :: Int -> (Int, Int) -> Int
linearFieldIndex w (i, j) = i + j * w

control :: Model -> (Double,Double) -> Bool -- не совсем верно - не учитывается зум и прочее
control m !(a,b) = not (cond halfX a || cond halfY b)
  where
    halfX = (fromIntegral $ mWidth m)/2
    halfY = (fromIntegral $ mHeight m)/2
    cond _ x = -- here was size
      isNaN x ||
      isInfinite x ||
      x <= - 1 ||
      x >=  1
-- | TODO alpha blending colours
calcColour :: (Double,Double,Double) -> Cell -> Cell
calcColour (r1,g1,b1) (r2, g2, b2, a) = ( (r2+r1), (g2+g1), (b2+b1), (a+1)) -- заглушка
--calcColour _ _ = Cell 1 0 0

fieldCellToPixel :: Int -> Field  -> Int -> Int -> PixelRGBA8
fieldCellToPixel width field  x y = toPixel $  field  Vector.! (linearFieldIndex width (x,y))
  where
    toPixel (r, g, b, a)= PixelRGBA8 nr ng nb 255
     where
      nr = fromIntegral $ round $ (r/a)*255
      ng = fromIntegral $ round $ (g/a)*255
      nb = fromIntegral $ round $ (b/a)*255

-- | Случайная точка из би-единичного квадрата:
randomBiUnitSquarePoint :: RandomGen g => g -> (Vec, g)
randomBiUnitSquarePoint g = ((x, y), g'')
  where
    (x, g')  = randomR (-1, 1) g
    (y, g'') = randomR (-1, 1) g'

randBUSlist :: RandomGen g => g -> [Vec]
randBUSlist gen = zip randXS randYS
  where
    (g1,g2) = split gen
    randXS = randomRs (-1,1) g1
    randYS = randomRs (-1,1) g2