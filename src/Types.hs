{-|
Module      : Types
Description : declarating types, implementation of main data, etc
Copyright   : Just Nothing
Stability   : in progress
-}
module Types (module Types, module GVector) where -- re-export GVector for Everyone using Types
import Prelude
import qualified Data.Vector.Unboxed as Vector
import GVector
import Gradient

-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)

-- | Бросок , цвет , номер трансформы
type CastGen = (GVec,Double,Int)

type Cell = (Double,Double,Double,Double)
type Field = Vector.Vector Cell
linearFieldIndex :: Int -> (Int, Int) -> Int
linearFieldIndex w (i, j) = i + j * w
{-# INLINE linearFieldIndex #-}

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
  vParams :: Params, -- ^ параметры
  vFunction :: VariationFunc -- ^ применяемое отображение
}
-- | Тождественная вариация
idVariation = Var {
    vScale    = 1
  , vParams   = Matrix idMatrix
  , vFunction = affineTransform
  }

-- | афинное преобразование
affineTransform :: VariationFunc 
affineTransform (Matrix m) g@(GVec _ (x,y)) = g {gvVec = (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m)}
affineTransform _ a = a

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
tName :: String,
-- | возможны линейные комбинации, композиция, параметры =>
-- variation :: VTree
tVariation :: Variation,  
tWeight :: Double,
-- ^ вес в вероятностном распределении
tColorPosition :: Double,
tColorSpeed :: Double,
-- ^ калибровка коэффициентов при смешении
tOpacity :: Double,
tXaos :: [Double]
}

templateTransform :: Transform
{-# INLINE templateTransform #-}
templateTransform = Transform {
                 tName          = "template"
               , tVariation     = idVariation
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

-- | Набоор параметров, однозначно задающих фрактал
data Model = Model {
  mName :: String,
  -- | череда трансформ
  mTransforms :: [Transform],
  -- viewPoint, условно
  mFinal :: Maybe Transform,
  -- | карта градиентов
  -- стоит сделать матрицей
  -- мб Data.Vector?
  mGradient :: [(Double,Double,Double)],
  -- | Размер картинки, 
  --backGrCol :: Cell
  mWidth, mHeight :: Int,
  -- | зум
  mScale :: Double,
  -- | смещение
  mShiftX, mShiftY :: Double,
  -- | и поворот.
  mRotation :: Double,
  mBackgroundColour :: (Int -> Cell),
  mOuterIter :: Int,
  mInnerIter :: Int
}

templateModel :: Model
{-# INLINE templateModel #-}
templateModel = Model {
    mName             = "42"
  , mTransforms       = []
  , mFinal           = Nothing
  , mGradient         = grad
  , mWidth            = 1024
  , mHeight           = 1024
  , mScale            = 1
  , mShiftX           = 0
  , mShiftY           = 0
  , mRotation         = 0
  , mBackgroundColour = (\_->(0,0,0,0))
  , mOuterIter        = 21845
  , mInnerIter        = 256
}
  where grad = paletteToDouble "\
\AA3B1CAA3F1C8144254454254D6D2551792F51712F5D713865854B758D5589A1\
\5E8DA254C2AD41DA8B2FDE7725D6672FD66A25C25F1C914C254C692534501220\
\4409203C122030122038122C48124069256189388DB25EAAC67AEED17AFAE154\
\E6972EE67A25E67A25DB6925CA5E25B64C1C853712542B09241B091C20001C30\
\09204009405C1C54793869914B91AE6796B27195AE718DAA71718D55516D4240\
\442F24341C1018120C0C090408000004000004000004000000000400000C0000\
\100B00240F00441712812612AA3712C25F1CB27C2F9AAE67A6C27AAACA7AB2D2\
\8DBEDAA0BED297CAD697E2DE84B6DA8DB6D28DBACE8DBECE97C6CEA9D2DAB3EA\
\E6C5EAEAC5FAF1BCF6E5BCEAC08DD2AC96BECA96B2C697B6CE8DB6CA8DB6C684\
\AACA84A6C6849EBA7A8DB27171954B5175383C542530401C1838091428091418\
\090C1009070C090408090804091407091C0F09241709502712812F12B23B12C6\
\5725DB6625DE6625DE751CE2871CE68E1CF29225FA9B1CF6BB1CEA7F2FFABC67\
\FAE170FAE154FED32EFECF2EFED04ADAB55E957867597942405D2F203C12142C\
\09041800000C0004080000040000040000040004000004000004040004040004\
\0800040800040800000400000000000009000000000000000000000000000000\
\0000000400000404000C0800101000201000381B095933097D4309A63F12B248\
\12C65A1CBE6B259D703899632F59793855653871481CB65025CE6225D26625D6\
\6E1CD66E1CDA7625DE6E1CE27225DE6E2FEE6F41C6A054A99470A1BA7AAAC284\
\BECE97CED6A0E2EEBCF6EEC5FEFAFEFAFECEF2FECEE6EEC5C6D6A0AAC2849195\
\71546D423840251C2C12182409141409201309300F09652712892F12A6331391\
\3712652F124427122038091C2C09141C09101409081000081000040C00041000\
\081409102009143012284812446D2561813889AA6795B67191B67181A15E6985\
\4B4459383C4C253040253C441C713712A63313BE4C1CCA5B1CD25E1CD2621CC6\
\57129E37096922002C0F001404000C00000804000408000810001020091C3409"