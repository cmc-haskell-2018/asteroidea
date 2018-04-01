{-|
Module      : Types
Description : declarating types, implementation of main data, etc
Copyright   : Just Nothing
Stability   : in progress
-}
module Types (module Types, module GVector) where -- re-export GVector for Everyone using Types
import Prelude
--import Control.Category
--import System.Random
import Data.Matrix
--import Graphics.Gloss
import GVector

-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)
-- | Бросок с привязанным генератором
type CastGen = (GVec,Double)

data Cell = Cell {getR :: Double, getG :: Double, getB :: Double, getA :: Double} 
  deriving (Show)


-- | Вариация как она есть, с параметрами, перевод GVec -> GVec
type VariationFunc =  Params -> GVec -> GVec --вместо Maybe Vec возможно стоит использовать Nan'ы 
-- | Любое отображение из R2 -> R
type Project = Vec ->  Double

{--
-- Категория значительно затуманивает устройство обёртки,
-- и усложняет добавление других данных к обёртке.
-- В ответ мы получаем лишь возможность использовать
-- знакомую точку (.) для композиции.
-- Так стоит ли это того?  
newtype Var a b = Var Params (Params->a->b)
instance Category Var where
  id = Var None (\_ a -> a)
-- @
-- (.) :: (Var p) b c -> (Var p) a b -> (Var p) a c
-- @
  (.) (Var p2 bc) (Var p1 ab) = Var None (\_ a -> bc p2 (ab p1 a))
-- композиция работает, пример:
-- >>> calcVariation (dbgAffine . dbgSpherical) (defGen , (1,1))
--}

-- | параметры для вариаций
data Params = None | List [Double] | Matrix AffineMatrix

-- | Обертка над VariationFunc - хранит ее параметры и скалярный множитель
data Variation = Var {
  vScale :: Double, -- ^ скалярный множитель
  params :: Params, -- ^ параметры
  function :: VariationFunc -- ^ применяемое отображение
}

-- | Матрицы афинных преобразований
data AffineMatrix = AffineMatrix {
  xx :: Double,
  
  yx :: Double,
  xy :: Double, -- | FIXME

  yy :: Double, 
  ox :: Double, 
  oy :: Double
} deriving(Show)

-- | тождественная матрица
idMatrix :: AffineMatrix
idMatrix = AffineMatrix 1 0 0 1 0 0

-- | Преобразование точки, цвета и всего такого
-- | По сути - Transform олицетворяет отображение из старой точки и цвета в новые точку и цвет
-- | Обычно это отображение точек - это сложное выражение из функций-вариаций
data Transform = Transform {
-- | Name ?
transformName :: String,
-- | возможны линейные комбинации, композиция, параметры =>
-- variation :: VTree
variation :: Variation,  
weight :: Double,
-- ^ вес в вероятностном распределении
colorPosition :: Double,
colorSpeed :: Double,
-- ^ калибровка коэффициентов при смешении
opacity :: Double,
xaos :: [Double]
}

-- | Набоор параметров, однозначно задающих фрактал
data Model = Model {
  modelName :: String,
  -- | череда трансформ
  tranforms :: [Transform],
  -- viewPoint, условно
  camera :: Maybe Transform,
  -- | карта градиентов
  -- стоит сделать матрицей
  -- мб Data.Vector?
  gradient :: [(Double,Double,Double)],
  -- | Размер картинки, 
  --backGrCol :: Cell
  width :: Int,
  height :: Int,
  -- | зум
  mScale :: Double,
  -- | и поворот.
  rotation :: Double
}
