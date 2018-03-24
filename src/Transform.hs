{-|
Module      : Transform
Description : operations with transform
Copyright   : Just Nothing
Stability   : in progress
-}
module Transform where

import Types
import Const
import GVector

-- | Выбор трансформы из списка
-- нашей модели
askTransform
  :: Model -- ^ модель
  -> Double-- ^ случайная величина [0,1)
  -> Transform
askTransform model choice =
  let
    thres = choice * sumWeight
    list = transforms model
  in findTrans (thres - (weight $ head list)) list
-- | перебор по списку до первого не превосходящего порог
findTrans
  :: Double -- ^ порог
  -> [Transform] -- ^ список
  -> Transform -- ^ результат
findTrans thres (x:y:xs)
  | (thres <= 0) = x
  | otherwise = findTrans (thres - (weight y)) (y:xs)
findTrans _ x = head x
-- | Применение трансформы
-- к цвету и вектору
applyTransform
  :: Transform
  -> Double   -- ^ цвет в карте градиентов
  -> GVec     -- ^ вектор
  -> (GVec, Double)
applyTransform transform colour gvec =
  let
    colSpeed = colorSpeed    transform
    colPosit = colorPosition transform
    newColour = (colour*abs(1+colSpeed) + colPosit*abs(1-colSpeed))/2
    variatTr = variation transform
    varFuncT = (function variatTr) $ (params variatTr)
  in (varFuncT gvec,newColour)