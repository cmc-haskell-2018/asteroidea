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
  :: Double-- ^ случайная величина [0,1)
  -> Transform
askTransform choice =
  let
    thres = choice * sumWeight
    list = transforms mainModel
  in findTrans (thres - (weight $ head list)) list
-- | перебор по списку до первого не превосходящего порог
findTrans
  :: Double -- ^ порог
  -> [Transform] -- ^ список
  -> Transform -- ^ результат
findTrans thres
  | (thres <= 0) = head
  | otherwise    = \(_:lst) -> findTrans (thres - (weight $ head lst)) lst
-- | Применение трансформы
-- к цвету и вектору
applyTransform
  :: Transform
  -> Double   -- ^ цвет в карте градиентов
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