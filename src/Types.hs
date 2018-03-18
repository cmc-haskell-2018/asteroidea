{-|
Module      : Types
Description : declarating types, implementation of main data, etc
Copyright   : Just Nothing
Stability   : in progress
-}
module Types where
import Prelude
--import Control.Category
import System.Random
import Data.Matrix
import Graphics.Gloss
--import Const

-- | Обёртка над Field, играющая роль мира. Без грязного IO.
data World =
  World {
    mugenga :: Field,  -- ^ 無限画
    getSGen :: StdGen, -- ^ standart pseudorandom number generator
    busList :: [Cast]  -- ^ BiUnitSquare coverage list
        }

-- | Вариация как она есть, с параметрами, перевод CastGen -> CastGen
type VariationFunc =  Params -> (StdGen,Vec) -> (StdGen,Vec) --вместо Maybe Vec возможно стоит использовать Nan'ы 
-- | Любое отображение из R2 -> R
type Project = Vec ->  Double
-- | Вектор Double
-- VS Gloss.Data.Point Float
-- вектор не привязан к СК.
type Vec = (Double, Double) 
-- | Поле есть матрица цветов
type Field = Matrix Color
-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)
-- | Бросок с привязанным генератором
type CastGen = ((Vec, Double),StdGen)

--42

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

-- | параметры для вариации
data Params = None | List [Double] | Matrix AffineMatrix
-- | Вариация
data Variation = Var {
  vScale :: Double, -- ^ ?
  params :: Params, -- ^ параметры
  function :: VariationFunc -- ^ применяемое отображение
}

-- | Матрицы афинных преобразований
data AffineMatrix = AffineMatrix {
  xx :: Double,
  xy :: Double, 
  yx :: Double, 
  yy :: Double, 
  ox :: Double, 
  oy :: Double
} deriving(Show)

-- | тождественное преобразование
idMatrix :: AffineMatrix
idMatrix = AffineMatrix 1 0 0 1 0 0

-- | Преобразование точки, цвета и всего такого
data Transform = Transform {
-- | Name ?
transformName :: String,
-- | возможны линейные комбинации, композиция, параметры =>
-- variation :: [ [(Double, Variation)] ] ??
variation :: Variation,  
weight :: Double,
-- ^ вес в вероятностном распределении
colorPosition :: Double,
colorSpeed :: Double,
-- ^ калибровка коэффициентов при смешении
opacity :: Double,
xaos :: [Double]
}

-- | Глобальный фрактал
data Model = Model {
  modelName :: String,
  -- | череда трансформ
  tranforms :: [Transform],
  -- viewPoint, условно
  camera :: Maybe Transform,
  -- | карта градиентов
  -- стоит сделать матрицей
  -- мб Data.Vector?
  gradient :: [Color],
  -- | Размер картинки, 
  width :: Int,
  height :: Int,
  -- | зум
  mScale :: Double,
  -- | и поворот.
  rotation :: Double
}
