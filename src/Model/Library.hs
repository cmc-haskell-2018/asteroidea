{-# LANGUAGE NegativeLiterals #-}
{-|
Module      : Library
Description : Collected fractal models
Copyright   : Just Nothing
Stability   : Stable
-}
module Model.Library(findModel, anyModel) where
import Types (Model(..))
import qualified Model.Serpinski
import qualified Model.Sphere
import qualified Model.Square
import qualified Model.Hex
import qualified Model.Tile
import Data.List

anyModel :: Model
anyModel = snd $ head mainList

findModel :: String -> Model
findModel sec = search temp
  where
    temp = find (\p -> sec == fst p) mainList
    search (Just res) = snd res
    search Nothing    = anyModel

mainList :: [(String,Model)]
mainList = map convert $ concat [l0,l1,l2,l3,l4]
  where
    l0 = Model.Serpinski.listModel
    l1 = Model.Sphere.listModel
    l2 = Model.Square.listModel
    l3 = Model.Tile.listModel
    l4 = Model.Hex.listModel

convert :: Model -> (String,Model)
convert model = (mName model, model)