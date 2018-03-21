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

askTransform
  :: Model
  -> Double
  -> Transform
askTransform model choice =
  let
    thres = choice * (sumweight model)
    list@(x:xs) = (transforms model)
  in findTrans (thres - (weight x)) list

findTrans :: Double -> [Transform] -> Transform
findTrans thres (x:y:xs)
  | (thres <= 0) = x
  | otherwise = findTrans (thres - (weight y)) (y:xs)
findTrans _ (x:xs) = x

applyTransform
  :: Transform
  -> Double
  -> GVec
  -> (GVec, Double)
applyTransform transform colour gvec =
  let
    colSpeed = colorSpeed    transform
    colPosit = colorPosition transform
    newColour = (colour*abs(1+colSpeed) + colPosit*abs(1-colSpeed))/2
    variatTr = variation transform
    varFuncT = (function variatTr) $ (params variatTr)
  in (varFuncT gvec,newColour)