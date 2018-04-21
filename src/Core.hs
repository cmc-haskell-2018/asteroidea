{-|
Module      : Core
Description : Module that contains all calculations
Copyright   : Just Nothing
Stability   : in progress
-}
module Core(calcFlame)  where
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
prepareModel :: Model -> Model
prepareModel model = model
  { mTransforms = map initCol $ map initOne list
  , mFinal      = initCol <$> final
  , mRotation   = (pi/180*) $ mRotation model
  }
  where
    list  = mTransforms model
    final = mFinal      model
    originWeights = map tWeight $ list
    initCol transform = transform
      { tColorSpeed = k
      , tColorPosition = c
      }
      where
        speed = tColorSpeed transform
        posit = tColorPosition transform
        k     = (1 + speed)/2
        c     = (1 - speed)*posit/2
    initOne transform = transform
      { tXaos
      = weightNormalize
      $ rankedWeights
      $ xaosWeights
      $ originWeights
      }
      where
        alter [] = id
        alter ll = zipWith (*) ll
        xaosWeights = (alter $ tXaos transform)
        rankedWeights weights = tail $ scanl (+) (0::Double) $ weights
        weightNormalize weights = map (/last weights) weights

-- | Calculate whole fractal
calcFlame :: Model -> Int -> [(Vec,Double,Transform)]
calcFlame rawModel seed = finalPoints
  where
    model = prepareModel rawModel
    outerIter = mOuterIter model -- внешний цикл
    pointList = take outerIter (randBUSlist seed) -- лист с точками что будем обсчитывать
    points    = concatMap (calcPath model) pointList
    finalize Nothing      = id
    finalize (Just final) = map (calcFinal final)
    camera = applyCamera model
    appcmr = \ (GVec _ vec, c,i) -> (camera vec, c, i)
    finalPoints =
           map appcmr
         $ finalize (mFinal model) points

-- | calculate Final transform
-- it doesn't change pointers
calcFinal :: Transform -> CastGen -> CastGen
calcFinal transform (gvec, colour, pointer)
  = (newGVec, newColour, pointer)
  where    
    newColour = calcCol transform colour
    newGVec  = tVariation transform $ gvec

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
calcOne model (gv, col, tran) =
  ( newGVec
  , newCol
  , newTran)
  where
    newGVec =  tVariation tran $ newGV
    newCol = calcCol tran col  
    (newTran, newGV) = calcPtr model (tran, gv)

-- | Calculate color
calcCol :: Transform -> Double -> Double
calcCol tran col = result
  where
    speed  = tColorSpeed tran
    posit  = tColorPosition tran
    result = speed * col + posit
    -- result = ((1 + speed)* col + (1 - speed)*posit)/2

-- | Calculate what transform will be used next
calcPtr :: Model -> (Transform, GVec) -> (Transform, GVec)
calcPtr m (tran, gv) = (newTran, newGV)
  where
    gen0 = gvGen gv
    (threshold, gen1) = randomR (0, 1) gen0
    newGV = gv {gvGen = gen1}
    index = fromJust $ findIndex (>= threshold) (tXaos tran)
    newTran = mTransforms m !! index

applyCamera :: Model -> Vec -> Vec
applyCamera m (x,y) = (x',y')
  where
    (shiftX, shiftY) = (x+ mShiftX m, y+ mShiftY m)
    rotRad = mRotation m
    sinT = scl * (sin rotRad)
    cosT = scl * (cos rotRad)
    scl = mScale m
    (rotX, rotY) = (shiftX*cosT-shiftY*sinT, shiftY*cosT+shiftX*sinT)
    (x',y') =(rotX,rotY)

-- | Список случайных точек из би-единичного квадрата:
randBUSlist :: Int -> [Vec]
randBUSlist seed = generate (-1,1) (RND seed)