{-|
Module      : Core
Description : Module that contains all calculations
Copyright   : Just Nothing
Stability   : in progress
-}
module Core(calcOne, calcFlame)  where
import Types
import RND
import Data.List
import Data.Maybe (fromJust)
-- import System.Random (StdGen, next, split, genRange)

{- | Если xaos в трансформе - пустой список,
то будем считать что переходы к любой другой трансформе равновозможны
xaos и веса не меняются в процессе вычисления =>
все необходимые "модифицированные" веса можно вычислить заранее
-}
initXaos :: Model -> Model
initXaos m@(Model {mTransforms = trs}) = m { mTransforms = map ini trs }
  where
    originWeights = map tWeight trs
    ini tr = tr {tXaos = list}
      where
        xaos = tXaos tr
        alter [] = id
        alter ll = zipWith (*) ll
        xaosWeights = (alter xaos) originWeights
        getRankedWeights = scanl (+) (0::Double) $ xaosWeights
        weightNormalCoef = (/last getRankedWeights)
        list = map weightNormalCoef getRankedWeights

-- | Calculate whole fractal
calcFlame :: Model -> StdGen -> [(Vec,Double,Transform)]
calcFlame model gen = finalestPoints
  where    
    pointList = take outerIter (randBUSlist gen) -- лист с точками что будем обсчитывать
    outerIter = mOuterIter model -- внешний цикл
    preparedModel = initXaos model
    points = concatMap (calcPath preparedModel) pointList
    finalFunc (Just final) = map (calc final)
    finalFunc Nothing      = id
    calc transform = let
      varGV =  (tVariation transform)
      speed = tColorSpeed transform
      posit = tColorPosition transform
      shift col = (
               (1 + speed)*col
              +
               (1 - speed)*posit
             ) /2
      in \(v,c,i) -> (varGV v, shift c, i)
    finalestPoints =
        map
        (  \ (GVec _ vec, c,i) -> ((applyCamera model vec), c, i) )
        (finalFunc (mFinal model) points)
-- | Calculate and plot Path of one point
calcPath ::  Model -> Vec -> [CastGen]
calcPath model vec = path
  where
    gen =  fromVec vec
    innerIter = mInnerIter model --  внутренний цикл
    start = (GVec gen vec, 0.5, head $ mTransforms model) -- here can be INITIAL transform
    infPath = iterate (calcOne model) start -- весь путь точки
    path = drop 20 $ take innerIter $ infPath 

-- | Calculate one point and color
calcOne :: Model -> CastGen -> CastGen
{-# INLINE calcOne #-}
calcOne model (gv, col, ptr) = (newGVec, newCol, newPtr)
  where
    transform = ptr
    gen0 = gvGen gv
    (threshold, gen1) = randomR (0, 1) gen0
    newGVec =  tVariation transform $ gv {gvGen = gen1}   
    newPtr = ((mTransforms model) !!) $ (-1 + ) $ fromJust
             $ findIndex
                 (>= threshold)
                 (tXaos transform)
    speed = tColorSpeed transform
    posit = tColorPosition transform
    newCol = (
               (1 + speed)*col
              +
               (1 - speed)*posit
             ) /2

applyCamera :: Model -> Vec -> Vec
applyCamera m (x,y) = (x',y')
  where
    (shiftX, shiftY) = (x+ mShiftX m, y+ mShiftY m)
    rotRad = (pi/180*) $ mRotation m
    sinT = sin rotRad
    cosT = cos rotRad
    scl = mScale m
    (rotX, rotY) = ( shiftX*cosT-shiftY*sinT, shiftY*cosT+shiftX*sinT)
    (x',y') =(rotX * scl,rotY * scl)

-- | Список случайных точек из би-единичного квадрата:
randBUSlist :: StdGen -> [Vec]
randBUSlist gen = zip randXS randYS
  where
    (g1,g2) = split gen
    randXS = randomRs (-1,1) g1
    randYS = randomRs (-1,1) g2