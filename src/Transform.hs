{-|
Module      : Transform
Description : operations with transform
Copyright   : Just Nothing
Stability   : in progress
-}
module Transform (getTransformNumber) where

import Types
--import Const
import System.Random

getTransformNumber
  :: [Transform]
  -> (Int, StdGen) 
  -> (Int, StdGen)
getTransformNumber transforms (ptr, gen) 
  | ptr < 0   = getSimple transforms gen
  | tXaos (transforms !! ptr) == [] = getSimple transforms gen -- if xaos is == []
  | otherwise = getWithXaos transforms (ptr, gen)


getSimple
  :: [Transform]
  -> StdGen
  -> (Int, StdGen)
getSimple transforms gen =
  (chooseTransform weights $ sumWeight * rand, gen')
  where
    (rand, gen') = randomR (0, 1) gen
    weights = map tWeight transforms
    sumWeight = sum weights

getWithXaos    
  :: [Transform]
  -> (Int, StdGen) 
  -> (Int, StdGen)
getWithXaos transforms (ptr, gen) =
  (chooseTransform weights $ sumWeight * rand, gen')
  where
    xaos = tXaos (transforms !! ptr)
    (rand, gen') = randomR (0, 1) gen
    weights = zipWith (*) xaos $ map tWeight transforms
    sumWeight = sum weights


chooseTransform
  :: [Double]
  -> Double
  -> Int
chooseTransform ( w : ws ) num
  | (num <= w) = 0
  | otherwise = 1 + chooseTransform ws (num - w)
{-
-- | Выбор трансформы из списка
-- нашей модели
askTransform
  :: Double-- ^ случайная величина [0,1)
  -> Transform
askTransform choice =
  let
    thres = choice * sumWeight
    list = transforms mainModel
  in findTransform (thres - (weight $ head list)) list
-- | перебор по списку до первого не превосходящего порог
findTransform
  :: Double -- ^ порог
  -> [Transform] -- ^ список
  -> Transform -- ^ результат
findTransform thres
  | (thres <= 0) = head
  | otherwise    = \(_:lst) -> findTransform (thres - (weight $ head lst)) lst
-- | Применение трансформы
-- к цвету и вектору
applyTransform
  :: Transform
  -> Double   -- ^ цвет в карте градиента
  -> GVec     -- ^ вектор
  -> (GVec, Double)
applyTransform transform colour =
  transform `seq` colour `seq`
  let
    colSpeed = colorSpeed    transform
    colPosit = colorPosition transform
    newColour = (colour*abs(1+colSpeed) + colPosit*abs(1-colSpeed))/2
    variatTr = variation transform
    varFuncT = (function variatTr) $ (params variatTr)
  in \gvec -> (varFuncT gvec, newColour)
-}
