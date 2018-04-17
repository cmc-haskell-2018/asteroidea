{-|
Module      : Core
Description : Module that contains all calculations
Copyright   : Just Nothing
Stability   : in progress
-}
module Core(calcOne, calcFlame)  where
import System.Random
import Types

-- | Если xaos в трансформе - пустой список, то будем считать что переходы к любой другой трансформе равновозможны
initXaos :: Model -> Model
initXaos m@(Model {mTransforms = trs}) = m { mTransforms = map ini trs }
 where
  ini tr | tXaos tr == [] = tr { tXaos =  replicate (length trs) 1 }
         | otherwise      = tr

-- | xaos и веса не меняются в процессе вычисления => все необходимые "модифицированные" веса можно вычислить заранее
prepareModel :: Model -> Model
prepareModel m@(Model {mTransforms = trs}) = m { mTransforms = preparedTransforms }
 where 
  preparedTransforms = map prepare trs
  prepare tr = tr { tXaos = zipWith (*) (tXaos tr) $ map tWeight trs}  

-- | Calculate whole fractal
calcFlame :: StdGen ->  Model -> [CastGen]
calcFlame gen model = concat $ map (calcPath $! preparedModel) pointList
  where    
    pointList = take outerIter (randBUSlist gen) -- лист с точками что будем обсчитывать
    outerIter = mOuterIter model -- внешний цикл, gо хорошему должен быть в модели
    preparedModel = prepareModel $ initXaos model

-- | Calculate and plot Path of one point
calcPath ::  Model->Vec->[CastGen]
calcPath  model vec@(x,y) = path
  where
    gen =  mkStdGen $ floor (100000 * (x+y))
    --(gen1,gen2) = split gen
    innerIter = mInnerIter model
    start = (GVec gen vec, 0.5, 0) -- here can be INITIAL transform
    infPath = iterate (\ c@(_,_,i) -> calcOne (mTransforms model !! i) c) start -- весь путь точки
    path = drop 20 $ take innerIter $ infPath --  внутренний цикл, по хорошему должен быть в модели

-- | Calculate one point and color
calcOne :: Transform -> CastGen -> CastGen
calcOne transform ( gv, col, ptr) = (newGVec, newCol, newPtr)
  where
    --(newPtr , newGen) = randomR (0, (length $ tXaos transform) -1 ) (gvGen gv)    
    (newPtr , newGen) = getTransformNumber transform (ptr, (gvGen gv))
    newGVec =  tVariation transform $ gv {gvGen = newGen}
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

getTransformNumber :: Transform -> (Int, StdGen) -> (Int, StdGen)
getTransformNumber transform (ptr, gen) 
  | tXaos transform == [] = (ptr, gen) -- if xaos is == [] t
  | otherwise = (chooseTransform list pointer, gen') 
   where
    (rand, gen') = randomR (0, 1) gen
    list = tXaos transform
    pointer = rand * sum list
{-# INLINE getTransformNumber #-}

chooseTransform :: [Double] -> Double -> Int
chooseTransform [] _ = 0
chooseTransform ( w : ws ) num
  | (num <= w) = 0
  | otherwise = 1 + chooseTransform ws (num - w)