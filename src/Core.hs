{-|
Module      : Core
Description : Module that contains all calculations
Copyright   : Just Nothing
Stability   : in progress
-}
module Core (calcFlame, calcOne) where
import System.Random
import Types
import Data.Vector.Storable (unsafeToForeignPtr)
import Const
import Gradient
import Variations
import Transform


-- | Calculate whole fractal
calcFlame :: StdGen ->  Model -> [CastGen]
calcFlame gen model = concat $ map (calcPath model) pointList
  where    
    pointList = take outerIter (randBUSlist gen) -- лист с точками что будем обсчитывать
    outerIter = mOuterIter model -- внешний цикл, gо хорошему должен быть в модели

-- | Calculate and plot Path of one point
calcPath ::  Model->Vec->[CastGen]
calcPath  model vec@(x,y) = path
  where
    gen =  mkStdGen $ floor (100000 * (x+y))
    --(gen1,gen2) = split gen
    innerIter = mInnerIter model
    start = (GVec gen vec, 0.5, -1) -- CastGen
    infPath = iterate (calcOne model) start -- весь путь точки
    path = drop 20 $ take innerIter $ infPath --  внутренний цикл, по хорошему должен быть в модели

-- | Calculate one point and color
calcOne :: Model -> CastGen -> CastGen
calcOne model ( gv, col, ptr)
  | ptr < 0   = (gv, col, newPtr) -- here can be INITIAL transform
  | otherwise = (newGVec, newCol, newPtr)
  where
    --(ptr , newGen) = randomR (0, (length $ mTransforms model) -1 ) gen    
    (newPtr , newGen) = Transform.getTransformNumber (mTransforms model) (ptr, (gvGen gv))
    transform = mTransforms model !! ptr 
    newGVec = calcVariation (tVariation transform) $ gv {gvGen = newGen}
    speed = tColorSpeed transform
    newCol = (
               (1 + speed)*col
              +
               (1 - speed)*(tColorPosition transform)
             ) /2  
{-# INLINE calcOne #-}

-- | Список случайных точек из би-единичного квадрата:
randBUSlist :: RandomGen g => g -> [Vec]
randBUSlist gen = zip randXS randYS
  where
    (g1,g2) = split gen
    randXS = randomRs (-1,1) g1
    randYS = randomRs (-1,1) g2
